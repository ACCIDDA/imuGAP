skip_if_not_installed("rstan")
#' "transformed_data/common_indices.stan" and "transformed_data/layer_phi_lookup.stan"
#' assemble precomputed index mappings (`obs_map_*`, `cdf_lookup_*`, `phi_lookup_*`)
#' across modular structural data includes (`data/shared.stan`, `data/locations.stan`,
#' and `data/*/weights_location.stan`).

targets <- c(
  "functions/bounds_to_range.stan",
  "functions/lookups.stan",
  "data/structural.stan",
  "data/locations.stan",
  "data/shared.stan",
  "transformed_data/common_indices.stan",
  "transformed_data/layer_phi_lookup.stan"
)

skip_if_stan_unchanged(targets)

model_common_indices <- sprintf(
  "
functions {
  #include functions/bounds_to_range.stan
  #include functions/lookups.stan
}
data {
  #include data/shared.stan
  #include data/locations.stan
  #include data/uncensored/weights_location.stan
  #include data/right/weights_location.stan
  #include data/left/weights_location.stan
}
transformed data {
  #include transformed_data/common_indices.stan
  #include transformed_data/layer_phi_lookup.stan
}
parameters {
  real dummy;
}
model {
  dummy ~ normal(0, 1);
}
generated quantities {
  array[2, n_obs_uncensored] int out_obs_map_unc = obs_map_uncensored;
  array[n_weights_uncensored] int out_cdf_lookup_unc = cdf_lookup_uncensored;
  array[n_weights_uncensored] int out_phi_lookup_unc = phi_lookup_uncensored;

  array[2, n_obs_right] int out_obs_map_right = obs_map_right;
  array[n_weights_right] int out_cdf_lookup_right = cdf_lookup_right;
  array[n_weights_right] int out_phi_lookup_right = phi_lookup_right;

  array[2, n_obs_left] int out_obs_map_left = obs_map_left;
  array[n_weights_left] int out_cdf_lookup_left = cdf_lookup_left;
  array[n_weights_left] int out_phi_lookup_left = phi_lookup_left;
}
"
) |>
  compile_stan_harness()

test_that("structural data and transformed indices create compliant lookup and range mappings", {
  n_yr <- 5L
  n_cohort <- 4L
  n_doses <- 2L
  n_locs <- 6L

  # Structural location hierarchy data
  n_layers <- 2L
  layer_starts <- c(1L, 2L)
  n_parent_locs <- 1L
  parent_loc_id <- 1L
  parent_child_starts <- 2L

  # Uncensored subset
  obs_bounds_unc <- c(1L, 3L)
  w_cohort_unc <- c(1L, 2L, 3L, 4L)
  w_dose_unc <- c(1L, 1L, 2L, 2L)
  w_life_year_unc <- c(1L, 2L, 1L, 3L)
  w_loc_unc <- c(1L, 2L, 3L, 4L)

  # Right-censored subset
  obs_bounds_right <- as.array(1L)
  w_cohort_right <- c(2L, 3L)
  w_dose_right <- c(1L, 2L)
  w_life_year_right <- c(2L, 4L)
  w_loc_right <- c(2L, 5L)

  # Left-censored subset
  obs_bounds_left <- as.array(1L)
  w_cohort_left <- c(1L, 4L)
  w_dose_left <- c(2L, 2L)
  w_life_year_left <- c(3L, 5L)
  w_loc_left <- c(3L, 6L)

  data_list <- list(
    # Structural parameters
    n_yr = n_yr,
    n_cohort = n_cohort,
    n_doses = n_doses,
    dose_sched = matrix(1, nrow = n_yr, ncol = n_doses),
    predict_mode = 0L,
    # Locations
    n_locs = n_locs,
    n_layers = n_layers,
    layer_starts = layer_starts,
    n_parent_locs = n_parent_locs,
    parent_loc_id = parent_loc_id,
    parent_child_starts = parent_child_starts,
    # Uncensored data
    n_obs_uncensored = length(obs_bounds_unc),
    y_obs_uncensored = rep(10L, length(obs_bounds_unc)),
    y_smp_uncensored = rep(20L, length(obs_bounds_unc)),
    n_weights_uncensored = length(w_dose_unc),
    obs_to_weights_bounds_uncensored = obs_bounds_unc,
    weights_cohort_uncensored = w_cohort_unc,
    weights_life_year_uncensored = w_life_year_unc,
    weights_dose_uncensored = w_dose_unc,
    weights_uncensored = rep(0.5, length(w_dose_unc)),
    weights_location_uncensored = w_loc_unc,
    # Right-censored data
    n_obs_right = length(obs_bounds_right),
    y_obs_right = rep(5L, length(obs_bounds_right)),
    y_smp_right = rep(15L, length(obs_bounds_right)),
    n_weights_right = length(w_dose_right),
    obs_to_weights_bounds_right = obs_bounds_right,
    weights_cohort_right = w_cohort_right,
    weights_life_year_right = w_life_year_right,
    weights_dose_right = w_dose_right,
    weights_right = rep(0.5, length(w_dose_right)),
    weights_location_right = w_loc_right,
    # Left-censored data
    n_obs_left = length(obs_bounds_left),
    y_obs_left = rep(3L, length(obs_bounds_left)),
    y_smp_left = rep(12L, length(obs_bounds_left)),
    n_weights_left = length(w_dose_left),
    obs_to_weights_bounds_left = obs_bounds_left,
    weights_cohort_left = w_cohort_left,
    weights_life_year_left = w_life_year_left,
    weights_dose_left = w_dose_left,
    weights_left = rep(0.5, length(w_dose_left)),
    weights_location_left = w_loc_left
  )

  results <- run_stan_harness(
    model_common_indices,
    data = data_list
  )

  # Uncensored range and lookups
  expect_equal(results$out_obs_map_unc[1, ], obs_bounds_unc)
  expect_equal(
    results$out_obs_map_unc[2, ],
    c(tail(obs_bounds_unc, -1) - 1L, length(w_dose_unc))
  )
  expect_equal(
    as.numeric(results$out_cdf_lookup_unc),
    w_life_year_unc + (w_dose_unc - 1L) * n_yr
  )
  expect_equal(
    as.numeric(results$out_phi_lookup_unc),
    w_cohort_unc + (w_loc_unc - 1L) * n_cohort
  )

  # Right-censored range and lookups
  expect_equal(results$out_obs_map_right[1, ], obs_bounds_right)
  expect_equal(
    results$out_obs_map_right[2, ],
    c(tail(obs_bounds_right, -1) - 1L, length(w_dose_right))
  )
  expect_equal(
    as.numeric(results$out_cdf_lookup_right),
    w_life_year_right + (w_dose_right - 1L) * n_yr
  )
  expect_equal(
    as.numeric(results$out_phi_lookup_right),
    w_cohort_right + (w_loc_right - 1L) * n_cohort
  )

  # Left-censored range and lookups
  expect_equal(results$out_obs_map_left[1, ], obs_bounds_left)
  expect_equal(
    results$out_obs_map_left[2, ],
    c(tail(obs_bounds_left, -1) - 1L, length(w_dose_left))
  )
  expect_equal(
    as.numeric(results$out_cdf_lookup_left),
    w_life_year_left + (w_dose_left - 1L) * n_yr
  )
  expect_equal(
    as.numeric(results$out_phi_lookup_left),
    w_cohort_left + (w_loc_left - 1L) * n_cohort
  )
})
