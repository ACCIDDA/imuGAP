skip_if_not_installed("rstan")
#' "generated_quantities/pointwise_log_lik.stan" defines
#' the pointwise observation log-likelihood evaluation in generated quantities.

target <- "generated_quantities/pointwise_log_lik.stan"

skip_if_stan_unchanged(c(
  "transformed_data/right/observations.stan",
  target
))

model_pointwise_ll <- sprintf(
  "
functions {
}
data {
  int<lower=0> n_obs_unmixed_uncensored;
  array[n_obs_unmixed_uncensored] int y_obs_unmixed_uncensored;
  array[n_obs_unmixed_uncensored] int y_smp_unmixed_uncensored;
  vector[n_obs_unmixed_uncensored] p_obs_unmixed_uncensored;

  int<lower=0> n_obs_mixed_uncensored;
  array[n_obs_mixed_uncensored] int y_obs_mixed_uncensored;
  array[n_obs_mixed_uncensored] int y_smp_mixed_uncensored;
  vector[n_obs_mixed_uncensored] p_obs_mixed_uncensored;

  int<lower=0> n_obs_unmixed_left;
  array[n_obs_unmixed_left] int y_obs_unmixed_left;
  array[n_obs_unmixed_left] int y_smp_unmixed_left;
  vector[n_obs_unmixed_left] p_obs_unmixed_left;

  int<lower=0> n_obs_mixed_left;
  array[n_obs_mixed_left] int y_obs_mixed_left;
  array[n_obs_mixed_left] int y_smp_mixed_left;
  vector[n_obs_mixed_left] p_obs_mixed_left;

  int<lower=0> n_obs_unmixed_right;
  array[n_obs_unmixed_right] int y_obs_unmixed_right;
  array[n_obs_unmixed_right] int y_smp_unmixed_right;
  vector[n_obs_unmixed_right] p_obs_unmixed_right;

  int<lower=0> n_obs_mixed_right;
  array[n_obs_mixed_right] int y_obs_mixed_right;
  array[n_obs_mixed_right] int y_smp_mixed_right;
  vector[n_obs_mixed_right] p_obs_mixed_right;
}
transformed data {
  #include transformed_data/right/observations.stan
}
parameters {
  real dummy;
}
model {
  dummy ~ normal(0, 1);
}
generated quantities {
  vector[
    n_obs_unmixed_uncensored + n_obs_mixed_uncensored +
    n_obs_unmixed_right + n_obs_mixed_right +
    n_obs_unmixed_left + n_obs_mixed_left
  ] log_lik;
  #include %s
}
",
  target
) |>
  compile_stan_harness()

test_that("pointwise_log_lik computes exact log probabilities across all streams", {
  d_list <- list(
    n_obs_unmixed_uncensored = 1L,
    y_obs_unmixed_uncensored = as.array(10L),
    y_smp_unmixed_uncensored = as.array(20L),
    p_obs_unmixed_uncensored = as.array(0.4),
    n_obs_mixed_uncensored = 1L,
    y_obs_mixed_uncensored = as.array(15L),
    y_smp_mixed_uncensored = as.array(30L),
    p_obs_mixed_uncensored = as.array(0.5),
    n_obs_unmixed_right = 1L,
    y_obs_unmixed_right = as.array(18L),
    y_smp_unmixed_right = as.array(20L),
    p_obs_unmixed_right = as.array(0.6),
    n_obs_mixed_right = 1L,
    y_obs_mixed_right = as.array(22L),
    y_smp_mixed_right = as.array(25L),
    p_obs_mixed_right = as.array(0.7),
    n_obs_unmixed_left = 1L,
    y_obs_unmixed_left = as.array(5L),
    y_smp_unmixed_left = as.array(25L),
    p_obs_unmixed_left = as.array(0.3),
    n_obs_mixed_left = 1L,
    y_obs_mixed_left = as.array(8L),
    y_smp_mixed_left = as.array(35L),
    p_obs_mixed_left = as.array(0.35)
  )

  ll <- run_stan_harness(
    model_pointwise_ll,
    data = d_list,
    pars = "log_lik"
  )

  expected_ll <- c(
    stats::dbinom(
      d_list$y_obs_unmixed_uncensored,
      d_list$y_smp_unmixed_uncensored,
      d_list$p_obs_unmixed_uncensored,
      log = TRUE
    ),
    stats::dbinom(
      d_list$y_obs_mixed_uncensored,
      d_list$y_smp_mixed_uncensored,
      d_list$p_obs_mixed_uncensored,
      log = TRUE
    ),
    stats::pbinom(
      d_list$y_smp_unmixed_right - d_list$y_obs_unmixed_right,
      d_list$y_smp_unmixed_right,
      1.0 - d_list$p_obs_unmixed_right,
      log.p = TRUE
    ),
    stats::pbinom(
      d_list$y_smp_mixed_right - d_list$y_obs_mixed_right,
      d_list$y_smp_mixed_right,
      1.0 - d_list$p_obs_mixed_right,
      log.p = TRUE
    ),
    stats::pbinom(
      d_list$y_obs_unmixed_left,
      d_list$y_smp_unmixed_left,
      d_list$p_obs_unmixed_left,
      log.p = TRUE
    ),
    stats::pbinom(
      d_list$y_obs_mixed_left,
      d_list$y_smp_mixed_left,
      d_list$p_obs_mixed_left,
      log.p = TRUE
    )
  )

  expect_equal(as.vector(ll), expected_ll, tolerance = 1e-10)
})
