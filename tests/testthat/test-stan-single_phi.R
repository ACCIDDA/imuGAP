skip_if_not_installed("rstan")
#' "model/single_phi.stan" evaluates
#' observation probabilities `p_obs_*` for single-location models by combining
#' cohort baseline spline effects with cumulative dose coverage.

target <- "model/single_phi.stan"

skip_if_stan_unchanged(c(
  "functions/unrolled_dose_static_lambda.stan",
  "functions/bounds_to_range.stan",
  "functions/lookups.stan",
  "data/shared.stan",
  "data/bspline.stan",
  "transformed_data/common_indices.stan",
  "transformed_data/single_phi_lookup.stan",
  "model/common_phi.stan",
  target
))

model_single_phi <- sprintf(
  "
functions {
  #include functions/unrolled_dose_static_lambda.stan
  #include functions/bounds_to_range.stan
  #include functions/lookups.stan
}
data {
  #include data/shared.stan
  #include data/bspline.stan

  // Deterministic parameter inputs passed via data for exact testing
  vector[k_bs] beta_bs;
  vector[n_doses] lambda_raw;
}
transformed data {
  #include transformed_data/common_indices.stan
  #include transformed_data/single_phi_lookup.stan
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

test_that("single_phi.stan computes observation probabilities accurately", {
  obs_bounds <- as.array(1L)
  w_cohort <- c(1L, 2L)
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
      y_obs_uncensored = as.array(rep(10L, length(obs_bounds))),
      y_smp_uncensored = as.array(rep(20L, length(obs_bounds))),
      n_weights_uncensored = length(w_cohort),
      obs_to_weights_bounds_uncensored = obs_bounds,
      weights_cohort_uncensored = w_cohort,
      weights_dose_uncensored = w_dose,
      weights_life_year_uncensored = w_life_year,
      weights_uncensored = weights
    ),
    empty_obs_stream("right"),
    empty_obs_stream("left"),
    list(
      k_bs = ncol(bs),
      bs = bs,
      beta_bs = beta_bs,
      lambda_raw = as.array(log(lambda_val))
    )
  )

  p_obs <- run_stan_harness(
    model_single_phi,
    data = data_list,
    p_obs_uncensored
  )

  # Analytical closed form expectation:
  phi_inv <- 1.0 - stats::plogis(0)
  cdfs <- 1.0 - exp(-lambda_val * w_life_year)
  expected_p <- sum(weights * phi_inv * cdfs)
  expect_equal(p_obs, expected_p, tolerance = 1e-6)
})
