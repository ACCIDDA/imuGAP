skip_if_not_installed("rstan")
# ' "model/censored.stan" defines
# ' the observation likelihood evaluating exact binomial log-probabilities for
# ' uncensored data and binomial CDF log-probabilities for interval-censored data.

target <- "model/censored.stan"

skip_if_stan_unchanged(target)

model_likelihood <- sprintf(
  "
data {
  int n_obs;
  int n_uncensored_obs;
  array[n_obs] int y_obs;
  array[n_obs] int y_smp;
  vector[n_obs] p_obs_in;
}
parameters {
  real dummy;
}
transformed parameters {
  vector[n_obs] p_obs = p_obs_in + dummy;
}
model {
  dummy ~ normal(0, 1);
  #include %s
}
",
  target
) |>
  compile_stan_harness()

test_that("censored.stan computes binomial log-likelihood for uncensored observations", {
  y_obs <- c(10L, 20L, 30L)
  y_smp <- c(20L, 40L, 50L)
  p_obs <- c(0.4, 0.5, 0.6)

  data_list <- list(
    n_obs = length(y_obs),
    n_uncensored_obs = length(y_obs),
    y_obs = y_obs,
    y_smp = y_smp,
    p_obs_in = p_obs
  )

  fit <- run_stan_harness(model_likelihood, data = data_list, return_fit = TRUE)
  lp <- rstan::log_prob(fit, upars = c(0.0), adjust_transform = FALSE)

  # In Stan, y_obs ~ binomial(...) computes unnormalized log-posterior (dropping lchoose):
  expected_total_lp <- sum(
    stats::dbinom(y_obs, size = y_smp, prob = p_obs, log = TRUE) -
      lchoose(y_smp, y_obs)
  )

  expect_equal(lp, expected_total_lp, tolerance = 1e-4)
})

test_that("censored.stan computes log-likelihood with censored observations", {
  # Obs 1 is uncensored, Obs 2 is censored (y_obs represents failure bound)
  y_obs <- c(10L, 5L)
  y_smp <- c(20L, 20L)
  p_obs <- c(0.4, 0.7)
  n_uncensored <- 1L

  data_list <- list(
    n_obs = length(y_obs),
    n_uncensored_obs = n_uncensored,
    y_obs = y_obs,
    y_smp = y_smp,
    p_obs_in = p_obs
  )

  fit <- run_stan_harness(model_likelihood, data = data_list, return_fit = TRUE)
  lp <- rstan::log_prob(fit, upars = c(0.0), adjust_transform = FALSE)

  # Obs 1: target += binomial_lpmf(y_obs[1] | y_smp[1], p_obs[1]) [includes lchoose]
  # Obs 2: target += binomial_lcdf(y_obs[2] | y_smp[2], 1 - p_obs[2])
  expected_total_lp <- stats::dbinom(y_obs[1], y_smp[1], p_obs[1], log = TRUE) +
    stats::pbinom(y_obs[2], y_smp[2], 1 - p_obs[2], log.p = TRUE)

  expect_equal(lp, expected_total_lp, tolerance = 1e-4)
})
