skip_if_not_installed("rstan")
# ' "transformed_data/layer_indices.stan" defines
# ' precomputed indexing structures (`obs_map`, `cohort_shift_counter`,
# ' `phi_lookup`, `cdf_lookup`, `loc_layer_idx`) for hierarchical multi-layer
# ' spatial models.

target <- "transformed_data/layer_indices.stan"

skip_if_stan_unchanged(c(
  "functions/bounds_to_range.stan",
  target
))

model_layer_indices <- sprintf(
  "
functions {
  #include functions/bounds_to_range.stan
}
data {
  int n_obs;
  int n_weights;
  int n_cohort;
  int n_yr;
  int n_locs;
  int n_layers;
  array[n_obs] int obs_to_weights_bounds;
  array[n_weights] int weights_cohort;
  array[n_weights] int weights_location;
  array[n_weights] int weights_dose;
  array[n_weights] int weights_life_year;
  array[2, n_layers] int layer_bounds;
  array[n_layers] int layer_sizes;
}
transformed data {
  #include %s
}
parameters {
  real dummy;
}
model {
  dummy ~ normal(0, 1);
}
generated quantities {
  array[2, n_obs] int out_obs_map = obs_map;
  vector[n_cohort] out_shift = cohort_shift_counter;
  array[n_weights] int out_phi_lookup = phi_lookup;
  array[n_weights] int out_cdf_lookup = cdf_lookup;
  array[n_locs - 1] int out_loc_layer_idx = loc_layer_idx;
}
",
  target
) |>
  compile_stan_harness()

test_that("layer_indices.stan constructs multi-layer mappings with canonical hierarchy", {
  data("locations_sim", package = "imuGAP")
  locs_sim <- canonicalize_locations(locations_sim)
  ld_sim <- assemble_layer_data(locs_sim)

  obs_bounds <- c(1L, 3L)
  w_cohort <- c(1L, 2L, 1L, 3L)
  w_loc <- c(2L, 2L, 6L, 7L)
  w_dose <- c(1L, 1L, 2L, 2L)
  w_life_year <- c(1L, 2L, 1L, 3L)
  n_cohort <- 3L
  n_yr <- 5L

  data_layer_list <- list(
    n_obs = length(obs_bounds),
    n_weights = length(w_cohort),
    n_cohort = n_cohort,
    n_yr = n_yr,
    n_locs = ld_sim$n_locs,
    n_layers = ld_sim$n_layers,
    obs_to_weights_bounds = obs_bounds,
    weights_cohort = w_cohort,
    weights_location = w_loc,
    weights_dose = w_dose,
    weights_life_year = w_life_year,
    layer_bounds = ld_sim$layer_bounds,
    layer_sizes = ld_sim$layer_sizes
  )

  res_layer <- run_stan_harness(model_layer_indices, data = data_layer_list)

  # Check obs_map
  obs_map <- res_layer$out_obs_map[1, , ]
  expect_equal(obs_map[1, ], obs_bounds)
  expect_equal(obs_map[2, ], c(tail(obs_bounds, -1) - 1L, length(w_cohort)))

  # Check cohort shift
  expect_equal(as.numeric(res_layer$out_shift[1, ]), seq_len(n_cohort))

  # Check loc_layer_idx: layer 2 locations -> layer 1; layer 3 locations -> layer 2
  expect_equal(
    as.numeric(res_layer$out_loc_layer_idx[1, ]),
    c(rep(1L, ld_sim$layer_sizes[2]), rep(2L, ld_sim$layer_sizes[3]))
  )

  # Check phi_lookup: cohort + (loc - 1) * n_cohort
  expect_equal(
    as.numeric(res_layer$out_phi_lookup[1, ]),
    w_cohort + (w_loc - 1L) * n_cohort
  )

  # Check cdf_lookup: life_year + (dose - 1) * n_yr
  expect_equal(
    as.numeric(res_layer$out_cdf_lookup[1, ]),
    w_life_year + (w_dose - 1L) * n_yr
  )
})
