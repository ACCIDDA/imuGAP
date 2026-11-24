skip_if_not_installed("rstan")
#' "functions/unrolled_dose_static_lambda.stan" defines overloads of
#' `unrolled_dose(int n_intervals, int n_doses, vector dt_vec, matrix dose_sched, lambda_raw)`
#' which compute cumulative vaccination coverage across intervals and doses
#' using a continuous-time Markov multi-state transition matrix exponential:
#'   - `vector lambda_raw` (length D): static log-rates -> vector (M * D)
#'   - `matrix lambda_raw` (M x D): log-rates varying by interval -> vector (M * D)
#'   - `array[] matrix lambda_raw` (G of M x D): per-group log-rates -> matrix (M * D) x G

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

# ---- Overloads: interval-varying and per-group hazards ----------------------------

model_unrolled_overloads <- sprintf(
  "
functions {
  #include %s
}
data {
  int n_intervals;
  int n_doses;
  int n_groups;
  vector[n_intervals] dt_vec;
  matrix[n_intervals, n_doses] dose_sched;
  vector[n_doses] lambda_vec;
  matrix[n_intervals, n_doses] lambda_mat;
  array[n_groups] matrix[n_intervals, n_doses] lambda_arr;
}
parameters {
  real dummy;
}
model {
  dummy ~ normal(0, 1);
}
generated quantities {
  vector[n_doses * n_intervals] out_vec = unrolled_dose(
    n_intervals, n_doses, dt_vec, dose_sched, lambda_vec
  );
  vector[n_doses * n_intervals] out_mat = unrolled_dose(
    n_intervals, n_doses, dt_vec, dose_sched, lambda_mat
  );
  matrix[n_doses * n_intervals, n_groups] out_arr = unrolled_dose(
    n_intervals, n_doses, dt_vec, dose_sched, lambda_arr
  );
}
",
  target
) |>
  compile_stan_harness()

#' R reference: CTMC propagation with interval-specific rates (lambda: M x D, natural scale)
reference_ctmc <- function(dt_vec, dose_sched, lambda) {
  n_doses <- ncol(dose_sched)
  n_states <- n_doses + 1L
  p_state <- c(1, rep(0, n_doses))
  out <- matrix(0, nrow = length(dt_vec), ncol = n_doses)
  for (m in seq_along(dt_vec)) {
    r <- dose_sched[m, ] * lambda[m, ] * dt_vec[m]
    q_mat <- matrix(0, n_states, n_states)
    for (k in seq_len(n_doses)) {
      q_mat[k, k] <- -r[k]
      q_mat[k, k + 1L] <- r[k]
    }
    # Matrix exponential via eigendecomposition of the upper-triangular generator
    # is unstable when rates coincide; use scaling-and-squaring Taylor series instead.
    nq <- max(rowSums(abs(q_mat)))
    s_pow <- max(0L, ceiling(log2(nq + 1e-12)))
    q_s <- q_mat / 2^s_pow
    e_mat <- diag(n_states)
    term <- diag(n_states)
    for (i in seq_len(25L)) {
      term <- (term %*% q_s) / i
      e_mat <- e_mat + term
    }
    for (i in seq_len(s_pow)) {
      e_mat <- e_mat %*% e_mat
    }
    p_state <- as.vector(p_state %*% e_mat)
    for (k in seq_len(n_doses)) {
      out[m, k] <- sum(p_state[(k + 1L):n_states])
    }
  }
  out
}

overload_case <- function() {
  dose_sched <- cbind(c(0, 1, 1, 1, 1, 1), c(0, 0, 0, 1, 1, 1))
  n_intervals <- nrow(dose_sched)
  lambda_vec <- log(c(2.8, 3.0))
  # Calendar-style disruption: slower in intervals 2-3, catch-up in interval 4
  mult <- c(1, 0.4, 0.4, 1.5, 1, 1)
  lambda_mat <- log(outer(mult, exp(lambda_vec)))
  lambda_arr <- array(
    0,
    dim = c(3L, n_intervals, 2L)
  )
  lambda_arr[1L, , ] <- matrix(lambda_vec, n_intervals, 2L, byrow = TRUE)
  lambda_arr[2L, , ] <- lambda_mat
  lambda_arr[3L, , ] <- log(outer(rev(mult), exp(lambda_vec)))
  list(
    n_intervals = n_intervals,
    n_doses = 2L,
    n_groups = 3L,
    dt_vec = rep(1.0, n_intervals),
    dose_sched = dose_sched,
    lambda_vec = lambda_vec,
    lambda_mat = lambda_mat,
    lambda_arr = lambda_arr
  )
}

test_that("matrix overload with constant rows is identical to the vector overload", {
  dat <- overload_case()
  dat$lambda_mat <- matrix(
    dat$lambda_vec,
    dat$n_intervals,
    dat$n_doses,
    byrow = TRUE
  )
  res <- run_stan_harness(
    model_unrolled_overloads,
    data = dat,
    c("out_vec", "out_mat")
  )
  expect_identical(res$out_mat, res$out_vec)
})

test_that("matrix overload matches CTMC propagation with interval-varying rates", {
  dat <- overload_case()
  res <- run_stan_harness(model_unrolled_overloads, data = dat, "out_mat")
  cov_mat <- matrix(res, nrow = dat$n_intervals, ncol = dat$n_doses)

  expected <- reference_ctmc(dat$dt_vec, dat$dose_sched, exp(dat$lambda_mat))
  expect_equal(cov_mat, expected, tolerance = 1e-10)

  # Dose 1 has a closed form: 1 - exp(-cumulative hazard)
  cum_haz <- cumsum(dat$dose_sched[, 1] * exp(dat$lambda_mat[, 1]) * dat$dt_vec)
  expect_equal(cov_mat[, 1], 1 - exp(-cum_haz), tolerance = 1e-12)

  # The disruption must actually change the result relative to static rates
  static <- reference_ctmc(
    dat$dt_vec,
    dat$dose_sched,
    matrix(exp(dat$lambda_vec), dat$n_intervals, dat$n_doses, byrow = TRUE)
  )
  expect_gt(max(abs(cov_mat - static)), 0.05)
})

test_that("array overload returns one column per group matching the matrix overload", {
  dat <- overload_case()
  res <- run_stan_harness(
    model_unrolled_overloads,
    data = dat,
    c("out_vec", "out_mat", "out_arr")
  )
  out_arr <- matrix(res$out_arr, nrow = dat$n_intervals * dat$n_doses)
  expect_identical(dim(out_arr), c(dat$n_intervals * dat$n_doses, dat$n_groups))

  # Group 1 = static rates, group 2 = lambda_mat
  expect_identical(out_arr[, 1L], res$out_vec)
  expect_identical(out_arr[, 2L], res$out_mat)

  # Group 3 = reversed disruption, checked against the reference
  expected_3 <- reference_ctmc(
    dat$dt_vec,
    dat$dose_sched,
    exp(dat$lambda_arr[3L, , ])
  )
  expect_equal(out_arr[, 3L], as.vector(expected_3), tolerance = 1e-10)
})
