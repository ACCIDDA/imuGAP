skip_if_not_installed("rstan")
#' "functions/unrolled_dose_static_lambda.stan" defines
#' `vector unrolled_dose(int n_yr, int n_doses, matrix dose_sched, vector lambda_raw)`
#' which computes cumulative vaccination coverage across ages and doses
#' using a continuous-time Markov multi-state transition matrix exponential.

target <- "functions/unrolled_dose_static_lambda.stan"

skip_if_stan_unchanged(target)

model_unrolled <- sprintf(
  "
functions {
  #include %s
}
data {
  int n_yr;
  int n_doses;
  matrix[n_yr, n_doses] dose_sched;
  vector[n_doses] lambda_raw;
}
parameters {
  real dummy;
}
model {
  dummy ~ normal(0, 1);
}
generated quantities {
  vector[n_doses * n_yr] out_unrolled_dose = unrolled_dose(
    n_yr, n_doses, dose_sched, lambda_raw
  );
}
",
  target
) |>
  compile_stan_harness()

test_that("unrolled_dose computes cumulative dose coverage correctly with dose constraints", {
  dose_sched <- matrix(
    c(
      1.0,
      1.0,
      1.0,
      1.0,
      0.0,
      1.0,
      1.0,
      1.0,
      0.0,
      0.0,
      1.0,
      1.0
    ),
    nrow = 4L,
    ncol = 3L
  )
  n_yr <- nrow(dose_sched)
  n_doses <- ncol(dose_sched)
  lambda_raw <- c(log(0.5), log(0.4), log(0.3))

  cdf <- run_stan_harness(
    model_unrolled,
    data = list(
      n_yr = n_yr,
      n_doses = n_doses,
      dose_sched = dose_sched,
      lambda_raw = lambda_raw
    ),
    out_unrolled_dose
  )

  expect_length(cdf, n_yr * n_doses)
  expect_true(all(cdf >= 0 & cdf <= 1))

  # Monotonicity across years for each dose
  cov_mat <- matrix(cdf, nrow = n_yr, ncol = n_doses)
  for (d in seq_len(n_doses)) {
    expect_true(all(diff(cov_mat[, d]) >= 0))
  }

  # Dose 2 coverage is 0 before eligibility (year 1)
  expect_equal(cov_mat[1, 2], 0)
  # Dose 3 coverage is 0 before eligibility (years 1 and 2)
  expect_equal(cov_mat[1:2, 3], c(0, 0))

  # Precedence constraint: dose(d) <= dose(d-1)
  for (d in 2:n_doses) {
    expect_true(all(cov_mat[, d] <= cov_mat[, d - 1L] + 1e-12))
  }

  # Dose 1 matches exact 1 - exp(-lambda_1 * t)
  exact_dose_1 <- 1 - exp(-0.5 * seq_len(n_yr))
  expect_equal(cov_mat[, 1], exact_dose_1, tolerance = 1e-10)
})

test_that("unrolled_dose matches exact continuous-time Markov transition propagation", {
  # Two doses, varying rates and schedules
  dose_sched <- matrix(
    c(
      1.0,
      1.0,
      1.0,
      0.0,
      1.0,
      1.0
    ),
    nrow = 3L,
    ncol = 2L
  )
  n_yr <- nrow(dose_sched)
  n_doses <- ncol(dose_sched)
  lambda_raw <- c(log(0.8), log(0.6))
  lambdas <- exp(lambda_raw)

  cdf <- run_stan_harness(
    model_unrolled,
    data = list(
      n_yr = n_yr,
      n_doses = n_doses,
      dose_sched = dose_sched,
      lambda_raw = lambda_raw
    ),
    out_unrolled_dose
  )

  cov_mat <- matrix(cdf, nrow = n_yr, ncol = n_doses)

  # Hand-compute exact state distribution over 3 years:
  # Year 1: only dose 1 active
  # p_state(1) = [exp(-0.8), 1 - exp(-0.8), 0]
  p0 <- c(1, 0, 0)
  Q1 <- matrix(
    c(
      -lambdas[1],
      lambdas[1],
      0,
      0,
      0,
      0,
      0,
      0,
      0
    ),
    nrow = 3,
    byrow = TRUE
  )
  p1 <- p0 %*% expm::expm(Q1)
  expect_equal(cov_mat[1, 1], sum(p1[2:3]), tolerance = 1e-10)
  expect_equal(cov_mat[1, 2], p1[3], tolerance = 1e-10)

  # Year 2: both doses active
  Q2 <- matrix(
    c(
      -lambdas[1],
      lambdas[1],
      0,
      0,
      -lambdas[2],
      lambdas[2],
      0,
      0,
      0
    ),
    nrow = 3,
    byrow = TRUE
  )
  p2 <- p1 %*% expm::expm(Q2)
  expect_equal(cov_mat[2, 1], sum(p2[2:3]), tolerance = 1e-10)
  expect_equal(cov_mat[2, 2], p2[3], tolerance = 1e-10)

  # Year 3: both doses active
  p3 <- p2 %*% expm::expm(Q2)
  expect_equal(cov_mat[3, 1], sum(p3[2:3]), tolerance = 1e-10)
  expect_equal(cov_mat[3, 2], p3[3], tolerance = 1e-10)
})

test_that("unrolled_dose supports simultaneous onset, fractional values, and deactivation", {
  dose_sched <- matrix(
    c(
      0.8,
      1.0,
      0.0,
      0.0,
      0.5,
      0.5,
      0.5,
      0.0
    ),
    nrow = 4L,
    ncol = 2L
  )
  n_yr <- nrow(dose_sched)
  n_doses <- ncol(dose_sched)
  lambda_raw <- c(log(0.6), log(0.3))

  cdf <- run_stan_harness(
    model_unrolled,
    data = list(
      n_yr = n_yr,
      n_doses = n_doses,
      dose_sched = dose_sched,
      lambda_raw = lambda_raw
    ),
    out_unrolled_dose
  )

  expect_length(cdf, n_yr * n_doses)
  expect_true(all(cdf >= 0 & cdf <= 1))

  cov_mat <- matrix(cdf, nrow = n_yr, ncol = n_doses)
  # Precedence constraint holds
  expect_true(all(cov_mat[, 2] <= cov_mat[, 1] + 1e-12))
})
