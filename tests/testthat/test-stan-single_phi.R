skip_if_not_installed("rstan")
# ' "model/single_phi.stan" evaluates
# ' observation probabilities `p_obs_*` for single-location models by combining
# ' cohort baseline spline effects with cumulative dose coverage.

target <- "model/single_phi.stan"

skip_if_stan_unchanged(c(
  "functions/diff.stan",
  "functions/unrolled_dose_static_lambda.stan",
  "functions/bounds_to_range.stan",
  "functions/lookups.stan",
  "transformed_data/common_indices.stan",
  "transformed_data/single_indices.stan",
  "model/common_phi.stan",
  target
))

model_single_phi <- sprintf(
  "
functions {
  #include functions/diff.stan
  #include functions/unrolled_dose_static_lambda.stan
  #include functions/bounds_to_range.stan
  #include functions/lookups.stan
}
data {
  int<lower=0> n_obs_uncensored;
  int<lower=0> n_weights_uncensored;
  array[n_obs_uncensored] int obs_to_weights_bounds_uncensored;
  array[n_weights_uncensored] int weights_cohort_uncensored;
  array[n_weights_uncensored] int weights_dose_uncensored;
  array[n_weights_uncensored] int weights_life_year_uncensored;
  vector[n_weights_uncensored] weights_uncensored;

  int<lower=0> n_obs_right;
  int<lower=0> n_weights_right;
  array[n_obs_right] int obs_to_weights_bounds_right;
  array[n_weights_right] int weights_cohort_right;
  array[n_weights_right] int weights_dose_right;
  array[n_weights_right] int weights_life_year_right;
  vector[n_weights_right] weights_right;

  int<lower=0> n_obs_left;
  int<lower=0> n_weights_left;
  array[n_obs_left] int obs_to_weights_bounds_left;
  array[n_weights_left] int weights_cohort_left;
  array[n_weights_left] int weights_dose_left;
  array[n_weights_left] int weights_life_year_left;
  vector[n_weights_left] weights_left;

  int n_cohort;
  int n_yr;

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
    n_obs_uncensored = length(obs_bounds),
    n_weights_uncensored = length(w_cohort),
    obs_to_weights_bounds_uncensored = obs_bounds,
    weights_cohort_uncensored = w_cohort,
    weights_dose_uncensored = w_dose,
    weights_life_year_uncensored = w_life_year,
    weights_uncensored = weights,
    n_obs_right = 0L,
    n_weights_right = 0L,
    obs_to_weights_bounds_right = integer(0),
    weights_cohort_right = integer(0),
    weights_dose_right = integer(0),
    weights_life_year_right = integer(0),
    weights_right = numeric(0),
    n_obs_left = 0L,
    n_weights_left = 0L,
    obs_to_weights_bounds_left = integer(0),
    weights_cohort_left = integer(0),
    weights_dose_left = integer(0),
    weights_life_year_left = integer(0),
    weights_left = numeric(0),
    n_cohort = nrow(bs),
    n_yr = nrow(dose_sched),
    k_bs = ncol(bs),
    bs = bs,
    n_doses = ncol(dose_sched),
    dose_sched = dose_sched,
    epsilon_p = 1e-9,
    beta_bs = beta_bs,
    lambda_raw = as.array(log(lambda_val))
  )

  p_obs <- run_stan_harness(model_single_phi, data = data_list, p_obs_uncensored)

  # Analytical closed form expectation:
  phi_inv <- 1.0 - stats::plogis(0)
  cdfs <- 1.0 - exp(-lambda_val * w_life_year)
  expected_p <- sum(weights * phi_inv * cdfs)
  expect_equal(p_obs, expected_p, tolerance = 1e-6)
})
