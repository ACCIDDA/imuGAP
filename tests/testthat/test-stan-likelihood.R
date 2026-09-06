test_that("Stan model/censored.stan computes binomial log-likelihood for uncensored observations", {
  skip_if_not_installed("rstan")
  skip_if_stan_unchanged("model/censored.stan")

  code <- "
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
    #include model/censored.stan
  }
  "

  y_obs <- c(10L, 20L, 30L)
  y_smp <- c(20L, 40L, 50L)
  p_obs <- c(0.4, 0.5, 0.6)

  data_list <- list(
    n_obs = 3L,
    n_uncensored_obs = 3L,
    y_obs = y_obs,
    y_smp = y_smp,
    p_obs_in = p_obs
  )

  fit <- run_stan_harness(code, data = data_list, return_fit = TRUE)
  lp <- rstan::log_prob(fit, upars = c(0.0), adjust_transform = FALSE)

  # In Stan, y_obs ~ binomial(...) computes unnormalized log-posterior (dropping lchoose):
  expected_total_lp <- sum(
    stats::dbinom(y_obs, size = y_smp, prob = p_obs, log = TRUE) -
      lchoose(y_smp, y_obs)
  )

  expect_equal(lp, expected_total_lp, tolerance = 1e-4)
})

test_that("Stan model/censored.stan computes log-likelihood with censored observations", {
  skip_if_not_installed("rstan")
  skip_if_stan_unchanged("model/censored.stan")

  code <- "
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
    #include model/censored.stan
  }
  "

  # Obs 1 is uncensored, Obs 2 is censored (y_obs represents failure bound)
  y_obs <- c(10L, 5L)
  y_smp <- c(20L, 20L)
  p_obs <- c(0.4, 0.7)

  data_list <- list(
    n_obs = 2L,
    n_uncensored_obs = 1L,
    y_obs = y_obs,
    y_smp = y_smp,
    p_obs_in = p_obs
  )

  fit <- run_stan_harness(code, data = data_list, return_fit = TRUE)
  lp <- rstan::log_prob(fit, upars = c(0.0), adjust_transform = FALSE)

  # Obs 1: target += binomial_lpmf(10 | 20, 0.4) [includes lchoose]
  # Obs 2: target += binomial_lcdf(5 | 20, 1 - 0.7) = pbinom(5, 20, 0.3, log.p = TRUE)
  expected_total_lp <- stats::dbinom(10, 20, 0.4, log = TRUE) +
    stats::pbinom(5, 20, 1 - 0.7, log.p = TRUE)

  expect_equal(lp, expected_total_lp, tolerance = 1e-4)
})
