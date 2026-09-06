test_that("Stan function bounds_to_range converts lower bounds to (lower, upper) pairs", {
  skip_if_not_installed("rstan")
  skip_if_stan_unchanged("functions/bounds_to_range.stan")

  code <- "
  functions {
    #include functions/bounds_to_range.stan
  }
  data {
    int N;
    array[N] int lowers;
    int ub;
  }
  parameters {
    real dummy;
  }
  model {
    dummy ~ normal(0, 1);
  }
  generated quantities {
    array[2, N] int res = bounds_to_range(lowers, ub);
  }
  "

  res <- run_stan_harness(
    code,
    data = list(N = 3L, lowers = c(1L, 5L, 10L), ub = 20L)
  )

  # res$res has shape [1, 2, 3] (iter, dim1, dim2)
  bounds <- res$res[1, , ]
  expect_equal(bounds[1, ], c(1, 5, 10))
  expect_equal(bounds[2, ], c(4, 9, 20))
})

test_that("Stan function element_mult_expand multiplies column by row vector", {
  skip_if_not_installed("rstan")
  skip_if_stan_unchanged("functions/element_mult_expand.stan")

  code <- "
  functions {
    #include functions/element_mult_expand.stan
  }
  data {
    int N_col;
    int N_row;
    vector[N_col] colv;
    row_vector[N_row] rowv;
  }
  parameters {
    real dummy;
  }
  model {
    dummy ~ normal(0, 1);
  }
  generated quantities {
    matrix[N_col, N_row] res = element_mult_expand(colv, rowv);
  }
  "

  colv <- c(2.0, 3.0)
  rowv <- c(1.0, 4.0, 5.0)
  res <- run_stan_harness(
    code,
    data = list(N_col = 2L, N_row = 3L, colv = colv, rowv = rowv)
  )

  mat <- res$res[1, , ]
  expected <- colv %*% t(rowv)
  expect_equal(mat, expected)
})

test_that("Stan function diff calculates sequential differences for vector and row_vector", {
  skip_if_not_installed("rstan")
  skip_if_stan_unchanged("functions/diff.stan")

  code <- "
  functions {
    #include functions/diff.stan
  }
  data {
    int N;
    vector[N] v;
    row_vector[N] rv;
  }
  parameters {
    real dummy;
  }
  model {
    dummy ~ normal(0, 1);
  }
  generated quantities {
    vector[N - 1] dv = diff(v);
    row_vector[N - 1] drv = diff(rv);
  }
  "

  v <- c(1.5, 3.5, 7.0, 12.0)
  res <- run_stan_harness(
    code,
    data = list(N = 4L, v = v, rv = v)
  )

  expected <- diff(v)
  expect_equal(as.numeric(res$dv[1, ]), expected)
  expect_equal(as.numeric(res$drv[1, ]), expected)
})

test_that("Stan functions colsum and rowsum compute matrix marginal sums", {
  skip_if_not_installed("rstan")
  skip_if_stan_unchanged("functions/matrix_sums.stan")

  code <- "
  functions {
    #include functions/matrix_sums.stan
  }
  data {
    int R;
    int C;
    matrix[R, C] M;
  }
  parameters {
    real dummy;
  }
  model {
    dummy ~ normal(0, 1);
  }
  generated quantities {
    row_vector[C] cs = colsum(M);
    vector[R] rs = rowsum(M);
  }
  "

  M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
  res <- run_stan_harness(
    code,
    data = list(R = 2L, C = 3L, M = M)
  )

  expect_equal(as.numeric(res$cs[1, ]), colSums(M))
  expect_equal(as.numeric(res$rs[1, ]), rowSums(M))
})

test_that("Stan function unrolled_dose computes cumulative dose coverage correctly", {
  skip_if_not_installed("rstan")
  skip_if_stan_unchanged(c(
    "functions/diff.stan",
    "functions/unrolled_dose_static_lambda.stan"
  ))

  code <- "
  functions {
    #include functions/diff.stan
    #include functions/unrolled_dose_static_lambda.stan
  }
  data {
    int n_yr;
    int n_doses;
    matrix[n_yr, n_doses] dose_sched;
    vector[n_doses] lambda_raw;
    real epsilon_p;
  }
  parameters {
    real dummy;
  }
  model {
    dummy ~ normal(0, 1);
  }
  generated quantities {
    vector[n_doses * n_yr] cdf = unrolled_dose(n_yr, n_doses, dose_sched, lambda_raw, epsilon_p);
  }
  "

  # Simple 2-dose, 3-year schedule
  n_yr <- 3L
  n_doses <- 2L
  dose_sched <- matrix(
    c(1.0, 1.0, 1.0, 0.5, 0.5, 0.5),
    nrow = n_yr,
    ncol = n_doses
  )
  lambda_raw <- c(log(0.5), log(0.3))

  res <- run_stan_harness(
    code,
    data = list(
      n_yr = n_yr,
      n_doses = n_doses,
      dose_sched = dose_sched,
      lambda_raw = lambda_raw,
      epsilon_p = 1e-9
    )
  )

  cdf <- as.numeric(res$cdf[1, ])
  expect_length(cdf, n_yr * n_doses)
  expect_true(all(cdf >= 0 & cdf <= 1))

  # Within dose 1 and dose 2, cumulative coverage should be non-decreasing across years
  dose1_cov <- cdf[1:n_yr]
  dose2_cov <- cdf[(n_yr + 1):(2 * n_yr)]
  expect_true(all(diff(dose1_cov) >= 0))
  expect_true(all(diff(dose2_cov) >= 0))

  # Dose 2 coverage cannot exceed dose 1 coverage at any life year
  expect_true(all(dose2_cov <= dose1_cov + 1e-12))
})
