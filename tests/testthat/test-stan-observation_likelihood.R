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
  int<lower=0> n_obs_unmixed_uncensored;
  array[n_obs_unmixed_uncensored] int y_obs_unmixed_uncensored;
  array[n_obs_unmixed_uncensored] int y_smp_unmixed_uncensored;
  vector[n_obs_unmixed_uncensored] p_obs_unmixed_uncensored_in;

  int<lower=0> n_obs_mixed_uncensored;
  array[n_obs_mixed_uncensored] int y_obs_mixed_uncensored;
  array[n_obs_mixed_uncensored] int y_smp_mixed_uncensored;
  vector[n_obs_mixed_uncensored] p_obs_mixed_uncensored_in;

  int<lower=0> n_obs_unmixed_left;
  array[n_obs_unmixed_left] int y_obs_unmixed_left;
  array[n_obs_unmixed_left] int y_smp_unmixed_left;
  vector[n_obs_unmixed_left] p_obs_unmixed_left_in;

  int<lower=0> n_obs_mixed_left;
  array[n_obs_mixed_left] int y_obs_mixed_left;
  array[n_obs_mixed_left] int y_smp_mixed_left;
  vector[n_obs_mixed_left] p_obs_mixed_left_in;

  int<lower=0> n_obs_unmixed_right;
  array[n_obs_unmixed_right] int y_obs_unmixed_right;
  array[n_obs_unmixed_right] int y_smp_unmixed_right;
  vector[n_obs_unmixed_right] p_obs_unmixed_right_in;

  int<lower=0> n_obs_mixed_right;
  array[n_obs_mixed_right] int y_obs_mixed_right;
  array[n_obs_mixed_right] int y_smp_mixed_right;
  vector[n_obs_mixed_right] p_obs_mixed_right_in;
}
transformed data {
  #include transformed_data/right/observations.stan
}
parameters {
  real dummy;
}
transformed parameters {
  vector[n_obs_unmixed_uncensored] p_obs_unmixed_uncensored = p_obs_unmixed_uncensored_in + dummy;
  vector[n_obs_mixed_uncensored] p_obs_mixed_uncensored = p_obs_mixed_uncensored_in + dummy;
  vector[n_obs_unmixed_left] p_obs_unmixed_left = p_obs_unmixed_left_in + dummy;
  vector[n_obs_mixed_left] p_obs_mixed_left = p_obs_mixed_left_in + dummy;
  vector[n_obs_unmixed_right] p_obs_unmixed_right = p_obs_unmixed_right_in + dummy;
  vector[n_obs_mixed_right] p_obs_mixed_right = p_obs_mixed_right_in + dummy;
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
      numeric(0),
      0L,
      integer(0),
      integer(0),
      numeric(0)
    ),
    paste0(
      c(
        "n_obs_unmixed_",
        "y_obs_unmixed_",
        "y_smp_unmixed_",
        "p_obs_unmixed_",
        "n_obs_mixed_",
        "y_obs_mixed_",
        "y_smp_mixed_",
        "p_obs_mixed_"
      ),
      tag,
      c("", "", "", "_in", "", "", "", "_in")
    )
  )
}

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
  d_list <- c(
    list(
      n_obs_unmixed_uncensored = 1L,
      y_obs_unmixed_uncensored = as.array(unc_data$y_obs[1]),
      y_smp_unmixed_uncensored = as.array(unc_data$y_smp[1]),
      p_obs_unmixed_uncensored_in = as.array(unc_data$p_obs[1]),
      n_obs_mixed_uncensored = 1L,
      y_obs_mixed_uncensored = as.array(unc_data$y_obs[2]),
      y_smp_mixed_uncensored = as.array(unc_data$y_smp[2]),
      p_obs_mixed_uncensored_in = as.array(unc_data$p_obs[2])
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
  expected_lp <- sum(stats::dbinom(
    unc_data$y_obs,
    unc_data$y_smp,
    unc_data$p_obs,
    log = TRUE
  ))
  expect_equal(lp_val, expected_lp, tolerance = 1e-6)
})

test_that("left-censored likelihood accumulates binomial LCDF values", {
  d_list <- c(
    empty_stream_in("uncensored"),
    list(
      n_obs_unmixed_left = 1L,
      y_obs_unmixed_left = as.array(lt_data$y_obs[1]),
      y_smp_unmixed_left = as.array(lt_data$y_smp[1]),
      p_obs_unmixed_left_in = as.array(lt_data$p_obs[1]),
      n_obs_mixed_left = 1L,
      y_obs_mixed_left = as.array(lt_data$y_obs[2]),
      y_smp_mixed_left = as.array(lt_data$y_smp[2]),
      p_obs_mixed_left_in = as.array(lt_data$p_obs[2])
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

test_that("right-censored likelihood accumulates failure binomial LCDF values", {
  d_list <- c(
    empty_stream_in("uncensored"),
    empty_stream_in("left"),
    list(
      n_obs_unmixed_right = 1L,
      y_obs_unmixed_right = as.array(rt_data$y_obs[1]),
      y_smp_unmixed_right = as.array(rt_data$y_smp[1]),
      p_obs_unmixed_right_in = as.array(rt_data$p_obs[1]),
      n_obs_mixed_right = 1L,
      y_obs_mixed_right = as.array(rt_data$y_obs[2]),
      y_smp_mixed_right = as.array(rt_data$y_smp[2]),
      p_obs_mixed_right_in = as.array(rt_data$p_obs[2])
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
