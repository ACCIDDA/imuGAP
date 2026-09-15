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
  w_cohort <- c(1L, 2L)
  w_dose <- c(1L, 1L)
  w_age <- c(1L, 2L)
  weights <- c(0.5, 0.5)

  bs <- matrix(c(1.0, 0.0, 0.0, 1.0), nrow = 2L, ncol = 2L)
  dose_sched <- matrix(c(1.0, 1.0), nrow = 2L, ncol = 1L)
  n_intervals <- nrow(dose_sched)
  beta_bs <- c(0.0, 0.0)
  lambda_val <- 1.0

  data_list <- c(
    list(
      n_yr = nrow(dose_sched),
      n_cohort = nrow(bs),
      n_doses = ncol(dose_sched),
      n_intervals = n_intervals,
      dt_vec = rep(1.0, n_intervals),
      dose_sched = dose_sched,
      age_to_interval_map = seq_len(nrow(dose_sched)),
      predict_mode = 0L,
      n_obs_unmixed_uncensored = 1L,
      y_obs_unmixed_uncensored = as.array(10L),
      y_smp_unmixed_uncensored = as.array(20L),
      w_cohort_unmixed_uncensored = as.array(1L),
      w_age_unmixed_uncensored = as.array(1L),
      w_dose_unmixed_uncensored = as.array(1L),
      n_obs_mixed_uncensored = 1L,
      y_obs_mixed_uncensored = as.array(10L),
      y_smp_mixed_uncensored = as.array(20L),
      n_weights_mixed_uncensored = length(w_cohort),
      obs_bounds_mixed_uncensored = as.array(1L),
      w_cohort_mixed_uncensored = w_cohort,
      w_age_mixed_uncensored = w_age,
      w_dose_mixed_uncensored = w_dose,
      weights_mixed_uncensored = weights
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

  p_mix <- run_stan_harness(
    model_single_phi,
    data = data_list,
    p_obs_mixed_uncensored
  )
  p_unmix <- run_stan_harness(
    model_single_phi,
    data = data_list,
    p_obs_unmixed_uncensored
  )

  # Analytical closed form expectation:
  phi_inv <- 1.0 - stats::plogis(0)
  cdfs <- 1.0 - exp(-lambda_val * w_age)
  expected_p_mix <- sum(weights * phi_inv * cdfs)
  expect_equal(p_mix, expected_p_mix, tolerance = 1e-6)

  expected_p_unmix <- phi_inv * (1.0 - exp(-lambda_val * 1.0))
  expect_equal(p_unmix, expected_p_unmix, tolerance = 1e-6)
})
