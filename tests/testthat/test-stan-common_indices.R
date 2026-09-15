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
  array[2, n_obs_mixed_uncensored] int out_obs_map_unc = obs_map_mixed_uncensored;
  array[n_obs_unmixed_uncensored] int out_cdf_lookup_unmix_unc = cdf_lookup_unmixed_uncensored;
  array[n_weights_mixed_uncensored] int out_cdf_lookup_mix_unc = cdf_lookup_mixed_uncensored;
  array[n_obs_unmixed_uncensored] int out_phi_lookup_unmix_unc = phi_lookup_unmixed_uncensored;
  array[n_weights_mixed_uncensored] int out_phi_lookup_mix_unc = phi_lookup_mixed_uncensored;

  array[2, n_obs_mixed_right] int out_obs_map_right = obs_map_mixed_right;
  array[n_obs_unmixed_right] int out_cdf_lookup_unmix_right = cdf_lookup_unmixed_right;
  array[n_weights_mixed_right] int out_cdf_lookup_mix_right = cdf_lookup_mixed_right;

  array[2, n_obs_mixed_left] int out_obs_map_left = obs_map_mixed_left;
  array[n_obs_unmixed_left] int out_cdf_lookup_unmix_left = cdf_lookup_unmixed_left;
  array[n_weights_mixed_left] int out_cdf_lookup_mix_left = cdf_lookup_mixed_left;
}
"
) |>
  compile_stan_harness()

test_that("structural data and transformed indices create compliant lookup and range mappings", {
  n_yr <- 5L
  n_cohort <- 4L
  n_doses <- 2L
  n_locs <- 6L
  n_intervals <- 5L
  age_to_interval_map <- seq_len(n_yr)

  # Structural location hierarchy data
  n_layers <- 2L
  layer_starts <- c(1L, 2L)
  n_parent_locs <- 1L
  parent_loc_id <- 1L
  parent_child_starts <- 2L

  # Uncensored subset: 1 unmixed observation and 1 mixed observation (2 weights)
  obs_unmix_unc <- 1L
  w_cohort_unmix_unc <- 1L
  w_dose_unmix_unc <- 1L
  w_age_unmix_unc <- 1L
  w_loc_unmix_unc <- 1L

  obs_bounds_mix_unc <- 1L
  w_cohort_mix_unc <- c(2L, 3L)
  w_dose_mix_unc <- c(1L, 2L)
  w_age_mix_unc <- c(2L, 1L)
  w_loc_mix_unc <- c(2L, 3L)

  data_list <- list(
    # Structural parameters
    n_yr = n_yr,
    n_cohort = n_cohort,
    n_doses = n_doses,
    n_intervals = n_intervals,
    dt_vec = rep(1.0, n_intervals),
    dose_sched = matrix(1, nrow = n_intervals, ncol = n_doses),
    age_to_interval_map = age_to_interval_map,
    predict_mode = 0L,
    # Locations
    n_locs = n_locs,
    n_layers = n_layers,
    layer_starts = layer_starts,
    n_parent_locs = n_parent_locs,
    parent_loc_id = as.array(parent_loc_id),
    parent_child_starts = as.array(parent_child_starts),
    loc_population = rep(1.0, n_locs),
    # Uncensored data
    n_obs_unmixed_uncensored = 1L,
    y_obs_unmixed_uncensored = as.array(10L),
    y_smp_unmixed_uncensored = as.array(20L),
    w_cohort_unmixed_uncensored = as.array(w_cohort_unmix_unc),
    w_age_unmixed_uncensored = as.array(w_age_unmix_unc),
    w_dose_unmixed_uncensored = as.array(w_dose_unmix_unc),
    w_loc_unmixed_uncensored = as.array(w_loc_unmix_unc),
    n_obs_mixed_uncensored = 1L,
    y_obs_mixed_uncensored = as.array(10L),
    y_smp_mixed_uncensored = as.array(20L),
    n_weights_mixed_uncensored = length(w_cohort_mix_unc),
    obs_bounds_mixed_uncensored = as.array(obs_bounds_mix_unc),
    w_cohort_mixed_uncensored = w_cohort_mix_unc,
    w_age_mixed_uncensored = w_age_mix_unc,
    w_dose_mixed_uncensored = w_dose_mix_unc,
    w_loc_mixed_uncensored = w_loc_mix_unc,
    weights_mixed_uncensored = rep(0.5, length(w_cohort_mix_unc)),
    # Right-censored data (empty)
    n_obs_unmixed_right = 0L,
    y_obs_unmixed_right = integer(0),
    y_smp_unmixed_right = integer(0),
    w_cohort_unmixed_right = integer(0),
    w_age_unmixed_right = integer(0),
    w_dose_unmixed_right = integer(0),
    w_loc_unmixed_right = integer(0),
    n_obs_mixed_right = 0L,
    y_obs_mixed_right = integer(0),
    y_smp_mixed_right = integer(0),
    n_weights_mixed_right = 0L,
    obs_bounds_mixed_right = integer(0),
    w_cohort_mixed_right = integer(0),
    w_age_mixed_right = integer(0),
    w_dose_mixed_right = integer(0),
    w_loc_mixed_right = integer(0),
    weights_mixed_right = numeric(0),
    # Left-censored data (empty)
    n_obs_unmixed_left = 0L,
    y_obs_unmixed_left = integer(0),
    y_smp_unmixed_left = integer(0),
    w_cohort_unmixed_left = integer(0),
    w_age_unmixed_left = integer(0),
    w_dose_unmixed_left = integer(0),
    w_loc_unmixed_left = integer(0),
    n_obs_mixed_left = 0L,
    y_obs_mixed_left = integer(0),
    y_smp_mixed_left = integer(0),
    n_weights_left = 0L,
    n_weights_mixed_left = 0L,
    obs_bounds_mixed_left = integer(0),
    w_cohort_mixed_left = integer(0),
    w_age_mixed_left = integer(0),
    w_dose_mixed_left = integer(0),
    w_loc_mixed_left = integer(0),
    weights_mixed_left = numeric(0)
  )

  results <- run_stan_harness(
    model_common_indices,
    data = data_list
  )

  # Uncensored range and lookups
  expect_equal(results$out_obs_map_unc[1, ], obs_bounds_mix_unc)
  expect_equal(
    results$out_obs_map_unc[2, ],
    c(tail(obs_bounds_mix_unc, -1) - 1L, length(w_cohort_mix_unc))
  )
  expect_equal(
    as.numeric(results$out_cdf_lookup_unmix_unc),
    age_to_interval_map[w_age_unmix_unc] + (w_dose_unmix_unc - 1L) * n_intervals
  )
  expect_equal(
    as.numeric(results$out_phi_lookup_unmix_unc),
    w_cohort_unmix_unc + (w_loc_unmix_unc - 1L) * n_cohort
  )
  expect_equal(
    as.numeric(results$out_cdf_lookup_mix_unc),
    age_to_interval_map[w_age_mix_unc] + (w_dose_mix_unc - 1L) * n_intervals
  )
  expect_equal(
    as.numeric(results$out_phi_lookup_mix_unc),
    w_cohort_mix_unc + (w_loc_mix_unc - 1L) * n_cohort
  )
})
