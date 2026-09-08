skip_if_not_installed("rstan")
# ' "transformed_data/layer_indices.stan" defines
# ' precomputed indexing structures (`obs_map_*`, `cohort_shift_counter`,
# ' `phi_lookup_*`, `cdf_lookup_*`, `loc_layer_idx`) for hierarchical multi-layer
# ' spatial models.

target <- "transformed_data/layer_indices.stan"

skip_if_stan_unchanged(c(
  "functions/bounds_to_range.stan",
  "functions/lookups.stan",
  "transformed_data/common_indices.stan",
  target
))

model_layer_indices <- sprintf(
  "
functions {
  #include functions/bounds_to_range.stan
  #include functions/lookups.stan
}
data {
  int<lower=0> n_obs_uncensored;
  int<lower=0> n_weights_uncensored;
  array[n_obs_uncensored] int obs_to_weights_bounds_uncensored;
  array[n_weights_uncensored] int weights_cohort_uncensored;
  array[n_weights_uncensored] int weights_location_uncensored;
  array[n_weights_uncensored] int weights_dose_uncensored;
  array[n_weights_uncensored] int weights_life_year_uncensored;

  int<lower=0> n_obs_right;
  int<lower=0> n_weights_right;
  array[n_obs_right] int obs_to_weights_bounds_right;
  array[n_weights_right] int weights_cohort_right;
  array[n_weights_right] int weights_location_right;
  array[n_weights_right] int weights_dose_right;
  array[n_weights_right] int weights_life_year_right;

  int<lower=0> n_obs_left;
  int<lower=0> n_weights_left;
  array[n_obs_left] int obs_to_weights_bounds_left;
  array[n_weights_left] int weights_cohort_left;
  array[n_weights_left] int weights_location_left;
  array[n_weights_left] int weights_dose_left;
  array[n_weights_left] int weights_life_year_left;

  int n_cohort;
  int n_yr;
  int n_locs;
  int n_layers;
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
  array[2, n_obs_uncensored] int out_obs_map_unc = obs_map_uncensored;
  vector[n_cohort] out_shift = cohort_shift_counter;
  array[n_weights_uncensored] int out_phi_lookup_unc = phi_lookup_uncensored;
  array[n_weights_uncensored] int out_cdf_lookup_unc = cdf_lookup_uncensored;
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
    n_obs_uncensored = length(obs_bounds),
    n_weights_uncensored = length(w_cohort),
    obs_to_weights_bounds_uncensored = obs_bounds,
    weights_cohort_uncensored = w_cohort,
    weights_location_uncensored = w_loc,
    weights_dose_uncensored = w_dose,
    weights_life_year_uncensored = w_life_year,
    n_obs_right = 0L,
    n_weights_right = 0L,
    obs_to_weights_bounds_right = integer(0),
    weights_cohort_right = integer(0),
    weights_location_right = integer(0),
    weights_dose_right = integer(0),
    weights_life_year_right = integer(0),
    n_obs_left = 0L,
    n_weights_left = 0L,
    obs_to_weights_bounds_left = integer(0),
    weights_cohort_left = integer(0),
    weights_location_left = integer(0),
    weights_dose_left = integer(0),
    weights_life_year_left = integer(0),
    n_cohort = n_cohort,
    n_yr = n_yr,
    n_locs = ld_sim$n_locs,
    n_layers = ld_sim$n_layers,
    layer_bounds = ld_sim$layer_bounds,
    layer_sizes = ld_sim$layer_sizes
  )

  obs_map <- run_stan_harness(model_layer_indices, data = data_layer_list, out_obs_map_unc)
  expect_equal(obs_map[1, ], obs_bounds)
  expect_equal(obs_map[2, ], c(tail(obs_bounds, -1) - 1L, length(w_cohort)))

  out_shift <- run_stan_harness(model_layer_indices, data = data_layer_list, out_shift)
  expect_equal(as.numeric(out_shift), seq_len(n_cohort))

  loc_layer_idx <- run_stan_harness(model_layer_indices, data = data_layer_list, out_loc_layer_idx)
  expect_equal(
    as.numeric(loc_layer_idx),
    c(rep(1L, ld_sim$layer_sizes[2]), rep(2L, ld_sim$layer_sizes[3]))
  )

  phi_lookup <- run_stan_harness(model_layer_indices, data = data_layer_list, out_phi_lookup_unc)
  expect_equal(
    as.numeric(phi_lookup),
    w_cohort + (w_loc - 1L) * n_cohort
  )

  cdf_lookup <- run_stan_harness(model_layer_indices, data = data_layer_list, out_cdf_lookup_unc)
  expect_equal(
    as.numeric(cdf_lookup),
    w_life_year + (w_dose - 1L) * n_yr
  )
})
