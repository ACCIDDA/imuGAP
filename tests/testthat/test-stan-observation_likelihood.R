skip_if_not_installed("rstan")
#' "model/observation_likelihood.stan" defines
#' the observation likelihood evaluating exact binomial log-probabilities for
#' uncensored data, binomial LCDF for left-censored data, and binomial LCDF for
#' right-censored failure count observations using reduce_sum.

target <- "model/observation_likelihood.stan"

skip_if_stan_unchanged(c(
  "functions/observation_likelihood_reduce.stan",
  "transformed_data/right/observations.stan",
  "model/uncensored/likelihood.stan",
  "model/right/likelihood.stan",
  "model/left/likelihood.stan",
  target
))

model_likelihood <- sprintf(
  "
functions {
  #include functions/observation_likelihood_reduce.stan
}
data {
  int<lower=0> n_phi;
  int<lower=0> n_unrolled;
  vector[n_phi] phi;
  vector[n_unrolled] unrolled_dose_probs;

  int<lower=0> n_obs_unmixed_uncensored;
  array[n_obs_unmixed_uncensored] int y_obs_unmixed_uncensored;
  array[n_obs_unmixed_uncensored] int y_smp_unmixed_uncensored;
  array[n_obs_unmixed_uncensored] int phi_lookup_unmixed_uncensored;
  array[n_obs_unmixed_uncensored] int cdf_lookup_unmixed_uncensored;

  int<lower=0> n_obs_mixed_uncensored;
  int<lower=0> n_weights_mixed_uncensored;
  array[n_obs_mixed_uncensored] int y_obs_mixed_uncensored;
  array[n_obs_mixed_uncensored] int y_smp_mixed_uncensored;
  array[2, n_obs_mixed_uncensored] int obs_map_mixed_uncensored;
  array[n_weights_mixed_uncensored] int phi_lookup_mixed_uncensored;
  array[n_weights_mixed_uncensored] int cdf_lookup_mixed_uncensored;
  vector[n_weights_mixed_uncensored] weights_mixed_uncensored;

  int<lower=0> n_obs_unmixed_left;
  array[n_obs_unmixed_left] int y_obs_unmixed_left;
  array[n_obs_unmixed_left] int y_smp_unmixed_left;
  array[n_obs_unmixed_left] int phi_lookup_unmixed_left;
  array[n_obs_unmixed_left] int cdf_lookup_unmixed_left;

  int<lower=0> n_obs_mixed_left;
  int<lower=0> n_weights_mixed_left;
  array[n_obs_mixed_left] int y_obs_mixed_left;
  array[n_obs_mixed_left] int y_smp_mixed_left;
  array[2, n_obs_mixed_left] int obs_map_mixed_left;
  array[n_weights_mixed_left] int phi_lookup_mixed_left;
  array[n_weights_mixed_left] int cdf_lookup_mixed_left;
  vector[n_weights_mixed_left] weights_mixed_left;

  int<lower=0> n_obs_unmixed_right;
  array[n_obs_unmixed_right] int y_obs_unmixed_right;
  array[n_obs_unmixed_right] int y_smp_unmixed_right;
  array[n_obs_unmixed_right] int phi_lookup_unmixed_right;
  array[n_obs_unmixed_right] int cdf_lookup_unmixed_right;

  int<lower=0> n_obs_mixed_right;
  int<lower=0> n_weights_mixed_right;
  array[n_obs_mixed_right] int y_obs_mixed_right;
  array[n_obs_mixed_right] int y_smp_mixed_right;
  array[2, n_obs_mixed_right] int obs_map_mixed_right;
  array[n_weights_mixed_right] int phi_lookup_mixed_right;
  array[n_weights_mixed_right] int cdf_lookup_mixed_right;
  vector[n_weights_mixed_right] weights_mixed_right;
  int<lower=1> num_threads;
}
transformed data {
  #include transformed_data/right/observations.stan
}
parameters {
  real dummy;
}
model {
  dummy ~ normal(0, 1);
  #include %s
}
",
  target
) |>
  compile_stan_harness()

empty_stream_in <- function(tag = c("uncensored", "right", "left")) {
  tag <- match.arg(tag)
  setNames(
    list(
      0L,
      integer(0),
      integer(0),
      integer(0),
      integer(0),
      0L,
      0L,
      integer(0),
      integer(0),
      matrix(0L, nrow = 2, ncol = 0),
      integer(0),
      integer(0),
      numeric(0)
    ),
    paste0(
      c(
        "n_obs_unmixed_",
        "y_obs_unmixed_",
        "y_smp_unmixed_",
        "phi_lookup_unmixed_",
        "cdf_lookup_unmixed_",
        "n_obs_mixed_",
        "n_weights_mixed_",
        "y_obs_mixed_",
        "y_smp_mixed_",
        "obs_map_mixed_",
        "phi_lookup_mixed_",
        "cdf_lookup_mixed_",
        "weights_mixed_"
      ),
      tag
    )
  )
}

base_data <- list(
  num_threads = 1L,
  n_phi = 2L,
  n_unrolled = 2L,
  phi = c(0.2, 0.5),
  unrolled_dose_probs = c(0.5, 1.0)
)
# For unmixed obs: (1 - phi[1]) * unrolled[1] = (1 - 0.2) * 0.5 = 0.4
# For mixed obs: weight=1.0 * (1 - phi[2]) * unrolled[2] = 1.0 * 0.5 * 1.0 = 0.5

unc_data <- list(
  y_obs = c(10L, 20L),
  y_smp = c(20L, 40L),
  p_obs = c(0.4, 0.5)
)

rt_data <- list(
  y_obs = c(15L, 18L),
  y_smp = c(20L, 30L),
  p_obs = c(0.4, 0.5)
)

lt_data <- list(
  y_obs = c(5L, 8L),
  y_smp = c(25L, 35L),
  p_obs = c(0.4, 0.5)
)

test_that("uncensored likelihood accumulates binomial log-probabilities via reduce_sum", {
  d_list <- c(
    base_data,
    list(
      n_obs_unmixed_uncensored = 1L,
      y_obs_unmixed_uncensored = as.array(unc_data$y_obs[1]),
      y_smp_unmixed_uncensored = as.array(unc_data$y_smp[1]),
      phi_lookup_unmixed_uncensored = as.array(1L),
      cdf_lookup_unmixed_uncensored = as.array(1L),
      n_obs_mixed_uncensored = 1L,
      n_weights_mixed_uncensored = 1L,
      y_obs_mixed_uncensored = as.array(unc_data$y_obs[2]),
      y_smp_mixed_uncensored = as.array(unc_data$y_smp[2]),
      obs_map_mixed_uncensored = matrix(c(1L, 1L), nrow = 2, ncol = 1),
      phi_lookup_mixed_uncensored = as.array(2L),
      cdf_lookup_mixed_uncensored = as.array(2L),
      weights_mixed_uncensored = as.array(1.0)
    ),
    empty_stream_in("left"),
    empty_stream_in("right")
  )

  lp <- run_stan_harness(
    model_likelihood,
    data = d_list,
    pars = NULL,
    return_fit = TRUE
  )
  lp_val <- rstan::log_prob(lp, 0.0, adjust_transform = FALSE)
  expected_lp <- sum(
    unc_data$y_obs *
      log(unc_data$p_obs) +
      (unc_data$y_smp - unc_data$y_obs) * log(1.0 - unc_data$p_obs)
  )
  expect_equal(lp_val, expected_lp, tolerance = 1e-6)
})

test_that("left-censored likelihood accumulates binomial LCDF values via reduce_sum", {
  d_list <- c(
    base_data,
    empty_stream_in("uncensored"),
    list(
      n_obs_unmixed_left = 1L,
      y_obs_unmixed_left = as.array(lt_data$y_obs[1]),
      y_smp_unmixed_left = as.array(lt_data$y_smp[1]),
      phi_lookup_unmixed_left = as.array(1L),
      cdf_lookup_unmixed_left = as.array(1L),
      n_obs_mixed_left = 1L,
      n_weights_mixed_left = 1L,
      y_obs_mixed_left = as.array(lt_data$y_obs[2]),
      y_smp_mixed_left = as.array(lt_data$y_smp[2]),
      obs_map_mixed_left = matrix(c(1L, 1L), nrow = 2, ncol = 1),
      phi_lookup_mixed_left = as.array(2L),
      cdf_lookup_mixed_left = as.array(2L),
      weights_mixed_left = as.array(1.0)
    ),
    empty_stream_in("right")
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

test_that("right-censored likelihood accumulates failure binomial LCDF values via reduce_sum", {
  d_list <- c(
    base_data,
    empty_stream_in("uncensored"),
    empty_stream_in("left"),
    list(
      n_obs_unmixed_right = 1L,
      y_obs_unmixed_right = as.array(rt_data$y_obs[1]),
      y_smp_unmixed_right = as.array(rt_data$y_smp[1]),
      phi_lookup_unmixed_right = as.array(1L),
      cdf_lookup_unmixed_right = as.array(1L),
      n_obs_mixed_right = 1L,
      n_weights_mixed_right = 1L,
      y_obs_mixed_right = as.array(rt_data$y_obs[2]),
      y_smp_mixed_right = as.array(rt_data$y_smp[2]),
      obs_map_mixed_right = matrix(c(1L, 1L), nrow = 2, ncol = 1),
      phi_lookup_mixed_right = as.array(2L),
      cdf_lookup_mixed_right = as.array(2L),
      weights_mixed_right = as.array(1.0)
    )
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
