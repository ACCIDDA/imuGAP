skip_if_not_installed("rstan")
# ' "model/single_phi.stan" evaluates
# ' observation probabilities `p_obs` for single-location models by combining
# ' cohort baseline spline effects with cumulative dose coverage.

target <- "model/single_phi.stan"

skip_if_stan_unchanged(c(
  "functions/diff.stan",
  "functions/unrolled_dose_static_lambda.stan",
  "functions/bounds_to_range.stan",
  "transformed_data/single_indices.stan",
  target
))

model_single_phi <- sprintf(
  "
functions {
  #include functions/diff.stan
  #include functions/unrolled_dose_static_lambda.stan
  #include functions/bounds_to_range.stan
}
data {
  int n_obs;
  int n_weights;
  int n_cohort;
  int n_yr;
  array[n_obs] int obs_to_weights_bounds;
  array[n_weights] int weights_cohort;
  array[n_weights] int weights_dose;
  array[n_weights] int weights_life_year;
  vector[n_weights] weights;

  int k_bs;
  matrix[n_cohort, k_bs] bs;
  int n_doses;
  matrix[n_yr, n_doses] dose_sched;
  real epsilon_p;

  vector[k_bs] beta_bs;
  vector[n_doses] lambda_raw;
}
transformed data {
  #include transformed_data/single_indices.stan
}
parameters {
  real dummy;
}
model {
  dummy ~ normal(0, 1);
}
generated quantities {
  vector[n_obs] p_obs;
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

  data_list <- list(
    n_obs = length(obs_bounds),
    n_weights = length(w_cohort),
    n_cohort = nrow(bs),
    n_yr = nrow(dose_sched),
    obs_to_weights_bounds = obs_bounds,
    weights_cohort = w_cohort,
    weights_dose = w_dose,
    weights_life_year = w_life_year,
    weights = weights,
    k_bs = ncol(bs),
    bs = bs,
    n_doses = ncol(dose_sched),
    dose_sched = dose_sched,
    epsilon_p = 1e-9,
    beta_bs = beta_bs,
    lambda_raw = as.array(log(lambda_val))
  )

  p_obs <- run_stan_harness(model_single_phi, data = data_list, p_obs)

  # Analytical closed form expectation:
  phi_inv <- 1.0 - stats::plogis(0)
  cdfs <- 1.0 - exp(-lambda_val * w_life_year)
  expected_p <- sum(weights * phi_inv * cdfs)
  expect_equal(p_obs, expected_p, tolerance = 1e-6)
})
