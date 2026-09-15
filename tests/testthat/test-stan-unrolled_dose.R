skip_if_not_installed("rstan")
#' "functions/unrolled_dose_static_lambda.stan" defines
#' `vector unrolled_dose(
#'   int n_intervals, int n_doses, vector dt_vec, matrix dose_sched, vector lambda_raw
#' )`
#' which computes cumulative vaccination coverage across intervals and doses
#' using a continuous-time Markov multi-state transition matrix exponential.

target <- "functions/unrolled_dose_static_lambda.stan"

skip_if_stan_unchanged(target)

model_unrolled <- sprintf(
  "
functions {
  #include %s
}
data {
  int n_intervals;
  int n_doses;
  vector[n_intervals] dt_vec;
  matrix[n_intervals, n_doses] dose_sched;
  vector[n_doses] lambda_raw;
}
parameters {
  real dummy;
}
model {
  dummy ~ normal(0, 1);
}
generated quantities {
  vector[n_doses * n_intervals] out_unrolled_dose = unrolled_dose(
    n_intervals, n_doses, dt_vec, dose_sched, lambda_raw
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
  n_intervals <- nrow(dose_sched)
  n_doses <- ncol(dose_sched)
  dt_vec <- rep(1.0, n_intervals)
  lambda_raw <- c(log(0.5), log(0.4), log(0.3))

  cdf <- run_stan_harness(
    model_unrolled,
    data = list(
      n_intervals = n_intervals,
      n_doses = n_doses,
      dt_vec = dt_vec,
      dose_sched = dose_sched,
      lambda_raw = lambda_raw
    ),
    out_unrolled_dose
  )

  expect_length(cdf, n_intervals * n_doses)
  expect_true(all(cdf >= 0 & cdf <= 1))

  # Monotonicity across intervals for each dose
  cov_mat <- matrix(cdf, nrow = n_intervals, ncol = n_doses)
  for (d in seq_len(n_doses)) {
    expect_true(all(diff(cov_mat[, d]) >= 0))
  }

  # Dose 2 coverage is 0 before eligibility (interval 1)
  expect_equal(cov_mat[1, 2], 0)
  # Dose 3 coverage is 0 before eligibility (intervals 1 and 2)
  expect_equal(cov_mat[1:2, 3], c(0, 0))

  # Precedence constraint: dose(d) <= dose(d-1)
  for (d in 2:n_doses) {
    expect_true(all(cov_mat[, d] <= cov_mat[, d - 1L] + 1e-12))
  }

  # Dose 1 matches exact 1 - exp(-lambda_1 * t)
  exact_dose_1 <- 1 - exp(-0.5 * seq_len(n_intervals))
  expect_equal(cov_mat[, 1], exact_dose_1, tolerance = 1e-10)
})

test_that("unrolled_dose matches continuous-time Markov transition propagation with variable dt", {
  # Two doses, varying rates and schedules across variable dt intervals
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
  n_intervals <- nrow(dose_sched)
  n_doses <- ncol(dose_sched)
  dt_vec <- c(1.0, 2.0, 3.0)
  lambda_raw <- c(log(0.8), log(0.6))
  lambdas <- exp(lambda_raw)

  cdf <- run_stan_harness(
    model_unrolled,
    data = list(
      n_intervals = n_intervals,
      n_doses = n_doses,
      dt_vec = dt_vec,
      dose_sched = dose_sched,
      lambda_raw = lambda_raw
    ),
    out_unrolled_dose
  )

  cov_mat <- matrix(cdf, nrow = n_intervals, ncol = n_doses)

  mat_exp <- function(q_mat) {
    norm_q <- max(rowSums(abs(q_mat)))
    k <- max(0L, ceiling(log2(norm_q + 1e-12)))
    q_scaled <- q_mat / (2^k)
    res <- diag(nrow(q_mat))
    term <- diag(nrow(q_mat))
    for (i in seq_len(20L)) {
      term <- (term %*% q_scaled) / i
      res <- res + term
    }
    for (i in seq_len(k)) {
      res <- res %*% res
    }
    res
  }

  # Hand-compute exact state distribution over 3 intervals:
  # Interval 1 (dt = 1.0): only dose 1 active
  p0 <- c(1, 0, 0)
  q1 <- matrix(
    c(
      -lambdas[1] * dt_vec[1],
      lambdas[1] * dt_vec[1],
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
  p1 <- p0 %*% mat_exp(q1)
  expect_equal(cov_mat[1, 1], sum(p1[2:3]), tolerance = 1e-10)
  expect_equal(cov_mat[1, 2], p1[3], tolerance = 1e-10)

  # Interval 2 (dt = 2.0): both doses active
  q2 <- matrix(
    c(
      -lambdas[1] * dt_vec[2],
      lambdas[1] * dt_vec[2],
      0,
      0,
      -lambdas[2] * dt_vec[2],
      lambdas[2] * dt_vec[2],
      0,
      0,
      0
    ),
    nrow = 3,
    byrow = TRUE
  )
  p2 <- p1 %*% mat_exp(q2)
  expect_equal(cov_mat[2, 1], sum(p2[2:3]), tolerance = 1e-10)
  expect_equal(cov_mat[2, 2], p2[3], tolerance = 1e-10)

  # Interval 3 (dt = 3.0): both doses active
  q3 <- matrix(
    c(
      -lambdas[1] * dt_vec[3],
      lambdas[1] * dt_vec[3],
      0,
      0,
      -lambdas[2] * dt_vec[3],
      lambdas[2] * dt_vec[3],
      0,
      0,
      0
    ),
    nrow = 3,
    byrow = TRUE
  )
  p3 <- p2 %*% mat_exp(q3)
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
  n_intervals <- nrow(dose_sched)
  n_doses <- ncol(dose_sched)
  dt_vec <- rep(1.0, n_intervals)
  lambda_raw <- c(log(0.6), log(0.3))

  cdf <- run_stan_harness(
    model_unrolled,
    data = list(
      n_intervals = n_intervals,
      n_doses = n_doses,
      dt_vec = dt_vec,
      dose_sched = dose_sched,
      lambda_raw = lambda_raw
    ),
    out_unrolled_dose
  )

  expect_length(cdf, n_intervals * n_doses)
  expect_true(all(cdf >= 0 & cdf <= 1))

  cov_mat <- matrix(cdf, nrow = n_intervals, ncol = n_doses)
  # Precedence constraint holds
  expect_true(all(cov_mat[, 2] <= cov_mat[, 1] + 1e-12))
})
