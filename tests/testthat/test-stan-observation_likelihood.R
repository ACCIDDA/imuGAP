skip_if_not_installed("rstan")
# ' "model/observation_likelihood.stan" defines
# ' the observation likelihood evaluating exact binomial log-probabilities for
# ' uncensored data, binomial LCDF for left-censored data, and binomial LCDF for
# ' right-censored failure count observations.

target <- "model/observation_likelihood.stan"

skip_if_stan_unchanged(c(
  "transformed_data/right/observations.stan",
  "model/uncensored/likelihood.stan",
  "model/right/likelihood.stan",
  "model/left/likelihood.stan",
  target
))

model_likelihood <- sprintf(
  "
data {
  int<lower=0> n_obs_uncensored;
  array[n_obs_uncensored] int y_obs_uncensored;
  array[n_obs_uncensored] int y_smp_uncensored;
  vector[n_obs_uncensored] p_obs_uncensored_in;

  int<lower=0> n_obs_left;
  array[n_obs_left] int y_obs_left;
  array[n_obs_left] int y_smp_left;
  vector[n_obs_left] p_obs_left_in;

  int<lower=0> n_obs_right;
  array[n_obs_right] int y_obs_right;
  array[n_obs_right] int y_smp_right;
  vector[n_obs_right] p_obs_right_in;
}
transformed data {
  #include transformed_data/right/observations.stan
}
parameters {
  real dummy;
}
transformed parameters {
  vector[n_obs_uncensored] p_obs_uncensored = p_obs_uncensored_in + dummy;
  vector[n_obs_left] p_obs_left = p_obs_left_in + dummy;
  vector[n_obs_right] p_obs_right = p_obs_right_in + dummy;
}
model {
  dummy ~ normal(0, 1);
  #include %s
}
",
  target
) |>
  compile_stan_harness()

unc_data <- list(
  y_obs = c(10L, 20L),
  y_smp = c(20L, 40L),
  p_obs = c(0.4, 0.5)
)

rt_data <- list(
  y_obs = c(15L, 18L),
  y_smp = c(20L, 30L),
  p_obs = c(0.6, 0.7)
)

lt_data <- list(
  y_obs = c(5L, 8L),
  y_smp = c(25L, 35L),
  p_obs = c(0.3, 0.25)
)

test_that("uncensored likelihood accumulates binomial log-probabilities", {
  d_list <- list(
    n_obs_uncensored = length(unc_data$y_obs),
    y_obs_uncensored = unc_data$y_obs,
    y_smp_uncensored = unc_data$y_smp,
    p_obs_uncensored_in = unc_data$p_obs,
    n_obs_left = 0L,
    y_obs_left = integer(0),
    y_smp_left = integer(0),
    p_obs_left_in = numeric(0),
    n_obs_right = 0L,
    y_obs_right = integer(0),
    y_smp_right = integer(0),
    n_weights_right = 0L,
    obs_to_weights_bounds_right = integer(0),
    weights_life_year_right = integer(0),
    weights_dose_right = integer(0),
    n_yr = 1L,
    p_obs_right_in = numeric(0)
  )

  lp <- run_stan_harness(
    model_likelihood,
    data = d_list,
    pars = NULL,
    return_fit = TRUE
  )
  lp_val <- rstan::log_prob(lp, 0.0, adjust_transform = FALSE)
  expected_lp <- sum(stats::dbinom(
    unc_data$y_obs,
    unc_data$y_smp,
    unc_data$p_obs,
    log = TRUE
  ))
  expect_equal(lp_val, expected_lp, tolerance = 1e-6)
})

test_that("left-censored likelihood accumulates binomial LCDF values", {
  d_list <- list(
    n_obs_uncensored = 0L,
    y_obs_uncensored = integer(0),
    y_smp_uncensored = integer(0),
    p_obs_uncensored_in = numeric(0),
    n_obs_left = length(lt_data$y_obs),
    y_obs_left = lt_data$y_obs,
    y_smp_left = lt_data$y_smp,
    p_obs_left_in = lt_data$p_obs,
    n_obs_right = 0L,
    y_obs_right = integer(0),
    y_smp_right = integer(0),
    n_weights_right = 0L,
    obs_to_weights_bounds_right = integer(0),
    weights_life_year_right = integer(0),
    weights_dose_right = integer(0),
    n_yr = 1L,
    p_obs_right_in = numeric(0)
  )

  lp <- run_stan_harness(
    model_likelihood,
    data = d_list,
    pars = NULL,
    return_fit = TRUE
  )
  lp_val <- rstan::log_prob(lp, 0.0, adjust_transform = FALSE)
  expected_lp <- sum(stats::pbinom(
    lt_data$y_obs,
    lt_data$y_smp,
    lt_data$p_obs,
    log.p = TRUE
  ))
  expect_equal(lp_val, expected_lp, tolerance = 1e-6)
})

test_that("right-censored likelihood accumulates failure binomial LCDF values", {
  d_list <- list(
    n_obs_uncensored = 0L,
    y_obs_uncensored = integer(0),
    y_smp_uncensored = integer(0),
    p_obs_uncensored_in = numeric(0),
    n_obs_left = 0L,
    y_obs_left = integer(0),
    y_smp_left = integer(0),
    p_obs_left_in = numeric(0),
    n_obs_right = length(rt_data$y_obs),
    y_obs_right = rt_data$y_obs,
    y_smp_right = rt_data$y_smp,
    n_weights_right = length(rt_data$y_obs),
    obs_to_weights_bounds_right = seq_along(rt_data$y_obs),
    weights_life_year_right = rep(1L, length(rt_data$y_obs)),
    weights_dose_right = rep(1L, length(rt_data$y_obs)),
    n_yr = 1L,
    p_obs_right_in = rt_data$p_obs
  )

  lp <- run_stan_harness(
    model_likelihood,
    data = d_list,
    pars = NULL,
    return_fit = TRUE
  )
  lp_val <- rstan::log_prob(lp, 0.0, adjust_transform = FALSE)
  y_fail <- rt_data$y_smp - rt_data$y_obs
  expected_lp <- sum(stats::pbinom(
    y_fail,
    rt_data$y_smp,
    1 - rt_data$p_obs,
    log.p = TRUE
  ))
  expect_equal(lp_val, expected_lp, tolerance = 1e-6)
})
