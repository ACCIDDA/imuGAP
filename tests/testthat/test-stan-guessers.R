skip_if_not_installed("rstan")

targets <- c(
  "functions/link/logit.stan",
  "functions/link/probit.stan",
  "functions/guess/zero.stan",
  "functions/guess/asymptotic.stan",
  "functions/guess/taylor_logit.stan",
  "functions/guess/taylor_probit.stan",
  "functions/guess/pade.stan",
  "functions/guess/conditioned_logit.stan",
  "functions/guess/conditioned_probit.stan",
  "functions/guess/dispatch_logit.stan",
  "functions/guess/dispatch_probit.stan"
)

skip_if_stan_unchanged(targets)

model_guess_logit <- sprintf(
  "
functions {
  #include functions/link/logit.stan
  #include functions/guess/dispatch_logit.stan
}
data {
  int<lower=1> C;
  int<lower=1> K;
  vector[C] p0;
  vector[K] w;
  vector[K] delta;
}
parameters {
  real dummy;
}
model {
  dummy ~ normal(0, 1);
}
generated quantities {
  vector[C] eta0 = link_fn(p0);
  vector[C] g_zero = guess_subpop_shift(eta0, p0, w, delta, 1);
  vector[C] g_taylor2 = guess_subpop_shift(eta0, p0, w, delta, 2);
  vector[C] g_taylor4 = guess_subpop_shift(eta0, p0, w, delta, 3);
  vector[C] g_pade = guess_subpop_shift(eta0, p0, w, delta, 4);
  vector[C] g_asymptotic = guess_subpop_shift(eta0, p0, w, delta, 5);
  vector[C] g_conditioned = guess_subpop_shift(eta0, p0, w, delta, 6);
  vector[C] g_default = guess_subpop_shift(eta0, p0, w, delta);
}
"
) |>
  compile_stan_harness()

model_guess_probit <- sprintf(
  "
functions {
  #include functions/link/probit.stan
  #include functions/guess/dispatch_probit.stan
}
data {
  int<lower=1> C;
  int<lower=1> K;
  vector[C] p0;
  vector[K] w;
  vector[K] delta;
}
parameters {
  real dummy;
}
model {
  dummy ~ normal(0, 1);
}
generated quantities {
  vector[C] eta0 = link_fn(p0);
  vector[C] g_zero = guess_subpop_shift(eta0, p0, w, delta, 1);
  vector[C] g_taylor2 = guess_subpop_shift(eta0, p0, w, delta, 2);
  vector[C] g_taylor4 = guess_subpop_shift(eta0, p0, w, delta, 3);
  vector[C] g_pade = guess_subpop_shift(eta0, p0, w, delta, 4);
  vector[C] g_asymptotic = guess_subpop_shift(eta0, p0, w, delta, 5);
  vector[C] g_conditioned = guess_subpop_shift(eta0, p0, w, delta, 6);
  vector[C] g_default = guess_subpop_shift(eta0, p0, w, delta);
}
"
) |>
  compile_stan_harness()

test_that("logit link guessers return consistent, finite shifts", {
  c_cohort <- 5L
  k_subpop <- 4L
  p0 <- seq(0.1, 0.9, length.out = c_cohort)
  w <- rep(1.0 / k_subpop, k_subpop)
  delta <- c(-0.6, -0.2, 0.3, 0.5)

  data_list <- list(
    C = c_cohort,
    K = k_subpop,
    p0 = p0,
    w = w,
    delta = delta
  )

  res <- run_stan_harness(model_guess_logit, data = data_list)

  # Zero guesser returns exact zeros
  expect_equal(res$g_zero, rep(0.0, c_cohort), tolerance = 1e-12)

  # Default 4-argument overload matches zero guesser
  expect_equal(res$g_default, res$g_zero)

  # All other guessers produce finite outputs
  expect_true(all(is.finite(res$g_taylor2)))
  expect_true(all(is.finite(res$g_taylor4)))
  expect_true(all(is.finite(res$g_pade)))
  expect_true(all(is.finite(res$g_asymptotic)))
  expect_true(all(is.finite(res$g_conditioned)))

  # For logit, pade falls back to taylor4
  expect_equal(res$g_pade, res$g_taylor4, tolerance = 1e-12)

  # At delta = 0, all guessers must return exact zero shifts
  data_zero_delta <- data_list
  data_zero_delta$delta <- rep(0.0, k_subpop)
  res_zero <- run_stan_harness(model_guess_logit, data = data_zero_delta)

  expect_equal(res_zero$g_taylor2, rep(0.0, c_cohort), tolerance = 1e-12)
  expect_equal(res_zero$g_taylor4, rep(0.0, c_cohort), tolerance = 1e-12)
  expect_equal(res_zero$g_asymptotic, rep(0.0, c_cohort), tolerance = 1e-12)
  expect_equal(res_zero$g_conditioned, rep(0.0, c_cohort), tolerance = 1e-12)
})

test_that("probit link guessers return consistent, finite shifts", {
  c_cohort <- 5L
  k_subpop <- 4L
  p0 <- seq(0.1, 0.9, length.out = c_cohort)
  w <- rep(1.0 / k_subpop, k_subpop)
  delta <- c(-0.6, -0.2, 0.3, 0.5)

  data_list <- list(
    C = c_cohort,
    K = k_subpop,
    p0 = p0,
    w = w,
    delta = delta
  )

  res <- run_stan_harness(model_guess_probit, data = data_list)

  # Zero guesser returns exact zeros
  expect_equal(res$g_zero, rep(0.0, c_cohort), tolerance = 1e-12)

  # Default 4-argument overload matches zero guesser
  expect_equal(res$g_default, res$g_zero)

  # All other guessers produce finite outputs
  expect_true(all(is.finite(res$g_taylor2)))
  expect_true(all(is.finite(res$g_taylor4)))
  expect_true(all(is.finite(res$g_pade)))
  expect_true(all(is.finite(res$g_asymptotic)))
  expect_true(all(is.finite(res$g_conditioned)))

  # At delta = 0, all guessers must return exact zero shifts
  data_zero_delta <- data_list
  data_zero_delta$delta <- rep(0.0, k_subpop)
  res_zero <- run_stan_harness(model_guess_probit, data = data_zero_delta)

  expect_equal(res_zero$g_taylor2, rep(0.0, c_cohort), tolerance = 1e-12)
  expect_equal(res_zero$g_taylor4, rep(0.0, c_cohort), tolerance = 1e-12)
  expect_equal(res_zero$g_pade, rep(0.0, c_cohort), tolerance = 1e-12)
  expect_equal(res_zero$g_asymptotic, rep(0.0, c_cohort), tolerance = 1e-12)
  expect_equal(res_zero$g_conditioned, rep(0.0, c_cohort), tolerance = 1e-12)
})
