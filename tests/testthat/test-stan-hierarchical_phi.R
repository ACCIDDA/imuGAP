skip_if_not_installed("rstan")
#' "model/hierarchical_phi.stan" evaluates
#' hierarchical spatial observation probabilities `p_obs_*` by accumulating
#' baseline spline effects and multi-layer spatial random walk offsets across
#' location hierarchies.

target <- "model/hierarchical_phi.stan"

skip_if_stan_unchanged(c(
  "functions/diff.stan",
  "functions/unrolled_dose_static_lambda.stan",
  "functions/bounds_to_range.stan",
  "functions/lookups.stan",
  "functions/layer_offsets.stan",
  "data/shared.stan",
  "data/locations.stan",
  "data/uncensored/weights_location.stan",
  "data/right/weights_location.stan",
  "data/left/weights_location.stan",
  "data/bspline.stan",
  "transformed_data/common_indices.stan",
  "transformed_data/layer_indices.stan",
  "transformed_data/layer_phi_lookup.stan",
  "model/common_phi.stan",
  target
))

model_hierarchical_phi <- sprintf(
  "
functions {
  #include functions/diff.stan
  #include functions/unrolled_dose_static_lambda.stan
  #include functions/bounds_to_range.stan
  #include functions/lookups.stan
  #include functions/layer_offsets.stan
}
data {
  #include data/shared.stan
  #include data/locations.stan
  #include data/uncensored/weights_location.stan
  #include data/right/weights_location.stan
  #include data/left/weights_location.stan
  #include data/bspline.stan
  real epsilon_p;

  // Deterministic parameter inputs passed via data for exact testing
  vector[k_bs] beta_bs;
  vector[n_doses] lambda_raw;
  vector[n_locs - 1] off_layer;
}
transformed data {
  #include transformed_data/common_indices.stan
  #include transformed_data/layer_indices.stan
  #include transformed_data/layer_phi_lookup.stan
}
parameters {
  real dummy;
}
model {
  dummy ~ normal(0, 1);
}
generated quantities {
  #include %s
}
",
  target
) |>
  compile_stan_harness()

test_that("hierarchical_phi.stan computes observation probabilities accurately", {
  data("locations_sim", package = "imuGAP")
  locs_sim <- canonicalize_locations(locations_sim)
  ld_sim <- assemble_layer_data(locs_sim)

  obs_bounds <- as.array(1L)
  w_cohort <- c(1L, 2L)
  w_loc <- c(2L, 2L)
  w_dose <- c(1L, 1L)
  w_life_year <- c(1L, 2L)
  weights <- c(0.5, 0.5)

  bs <- matrix(c(1.0, 0.0, 0.0, 1.0), nrow = 2L, ncol = 2L)
  dose_sched <- matrix(c(1.0, 1.0), nrow = 2L, ncol = 1L)
  beta_bs <- c(0.0, 0.0)
  lambda_val <- 1.0

  data_list <- c(
    list(
      n_yr = nrow(dose_sched),
      n_cohort = nrow(bs),
      n_doses = ncol(dose_sched),
      dose_sched = dose_sched,
      predict_mode = 0L,
      n_obs_uncensored = length(obs_bounds),
      y_obs_uncensored = rep(10L, length(obs_bounds)),
      y_smp_uncensored = rep(20L, length(obs_bounds)),
      n_weights_uncensored = length(w_cohort),
      obs_to_weights_bounds_uncensored = obs_bounds,
      weights_cohort_uncensored = w_cohort,
      weights_location_uncensored = w_loc,
      weights_dose_uncensored = w_dose,
      weights_life_year_uncensored = w_life_year,
      weights_uncensored = weights,
      n_obs_right = 0L,
      y_obs_right = integer(0),
      y_smp_right = integer(0),
      n_weights_right = 0L,
      obs_to_weights_bounds_right = integer(0),
      weights_cohort_right = integer(0),
      weights_location_right = integer(0),
      weights_dose_right = integer(0),
      weights_life_year_right = integer(0),
      weights_right = numeric(0),
      n_obs_left = 0L,
      y_obs_left = integer(0),
      y_smp_left = integer(0),
      n_weights_left = 0L,
      obs_to_weights_bounds_left = integer(0),
      weights_cohort_left = integer(0),
      weights_location_left = integer(0),
      weights_dose_left = integer(0),
      weights_life_year_left = integer(0),
      weights_left = numeric(0)
    ),
    ld_sim,
    list(
      k_bs = ncol(bs),
      bs = bs,
      epsilon_p = 1e-9,
      beta_bs = beta_bs,
      lambda_raw = as.array(log(lambda_val)),
      off_layer = rep(0.0, ld_sim$n_locs - 1L)
    )
  )

  p_obs <- run_stan_harness(
    model_hierarchical_phi,
    data = data_list,
    p_obs_uncensored
  )

  expect_length(p_obs, length(obs_bounds))
  expect_true(p_obs > 0 && p_obs < 1)

  # Analytical formula check:
  phi_inv <- 1.0 - stats::plogis(0)
  cdfs <- 1.0 - exp(-lambda_val * w_life_year)
  expected_p <- sum(weights * phi_inv * cdfs)
  expect_equal(p_obs, expected_p, tolerance = 1e-6)
})
