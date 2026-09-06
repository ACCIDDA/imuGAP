skip_if_not_installed("rstan")
skip_if_stan_unchanged(c(
  "functions/bounds_to_range.stan",
  "functions/element_mult_expand.stan",
  "functions/diff.stan",
  "functions/matrix_sums.stan",
  "functions/unrolled_dose_static_lambda.stan"
))

code_functions <- "
functions {
  #include functions/bounds_to_range.stan
  #include functions/diff.stan
  #include functions/element_mult_expand.stan
  #include functions/matrix_sums.stan
  #include functions/unrolled_dose_static_lambda.stan
}
data {
  // bounds_to_range
  int N_bounds;
  array[N_bounds] int lowers;
  int ub;
  // element_mult_expand
  int N_col;
  int N_row;
  vector[N_col] colv;
  row_vector[N_row] rowv;
  // diff
  int N_diff;
  vector[N_diff] v_diff;
  row_vector[N_diff] rv_diff;
  // matrix_sums
  int R_mat;
  int C_mat;
  matrix[R_mat, C_mat] M_sum;
  // unrolled_dose
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
  array[2, N_bounds] int out_bounds = bounds_to_range(lowers, ub);
  matrix[N_col, N_row] out_mult_expand = element_mult_expand(colv, rowv);
  vector[N_diff - 1] out_diff_v = diff(v_diff);
  row_vector[N_diff - 1] out_diff_rv = diff(rv_diff);
  row_vector[C_mat] out_colsum = colsum(M_sum);
  vector[R_mat] out_rowsum = rowsum(M_sum);
  vector[n_doses * n_yr] out_unrolled_dose = unrolled_dose(n_yr, n_doses, dose_sched, lambda_raw, epsilon_p);
}
"

model_functions <- compile_stan_harness(code_functions)

# Shared test inputs
colv_test <- c(2.0, 3.0)
rowv_test <- c(1.0, 4.0, 5.0)
v_test <- c(1.5, 3.5, 7.0, 12.0)
M_test <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
n_yr_test <- 3L
n_doses_test <- 2L
dose_sched_test <- matrix(
  c(1.0, 1.0, 1.0, 0.5, 0.5, 0.5),
  nrow = n_yr_test,
  ncol = n_doses_test
)
lambda_raw_test <- c(log(0.5), log(0.3))

data_functions <- list(
  N_bounds = 3L,
  lowers = c(1L, 5L, 10L),
  ub = 20L,
  N_col = 2L,
  N_row = 3L,
  colv = colv_test,
  rowv = rowv_test,
  N_diff = 4L,
  v_diff = v_test,
  rv_diff = v_test,
  R_mat = 2L,
  C_mat = 3L,
  M_sum = M_test,
  n_yr = n_yr_test,
  n_doses = n_doses_test,
  dose_sched = dose_sched_test,
  lambda_raw = lambda_raw_test,
  epsilon_p = 1e-9
)

res_fn <- run_stan_harness(model_functions, data = data_functions)

test_that("Stan function bounds_to_range converts lower bounds to (lower, upper) pairs", {
  bounds <- res_fn$out_bounds[1, , ]
  expect_equal(bounds[1, ], c(1, 5, 10))
  expect_equal(bounds[2, ], c(4, 9, 20))
})

test_that("Stan function element_mult_expand multiplies column by row vector", {
  mat <- res_fn$out_mult_expand[1, , ]
  expected <- colv_test %*% t(rowv_test)
  expect_equal(mat, expected)
})

test_that("Stan function diff calculates sequential differences for vector and row_vector", {
  expected <- diff(v_test)
  expect_equal(as.numeric(res_fn$out_diff_v[1, ]), expected)
  expect_equal(as.numeric(res_fn$out_diff_rv[1, ]), expected)
})

test_that("Stan functions colsum and rowsum compute matrix marginal sums", {
  expect_equal(as.numeric(res_fn$out_colsum[1, ]), colSums(M_test))
  expect_equal(as.numeric(res_fn$out_rowsum[1, ]), rowSums(M_test))
})

test_that("Stan function unrolled_dose computes cumulative dose coverage correctly", {
  cdf <- as.numeric(res_fn$out_unrolled_dose[1, ])
  expect_length(cdf, n_yr_test * n_doses_test)
  expect_true(all(cdf >= 0 & cdf <= 1))

  # Within dose 1 and dose 2, cumulative coverage should be non-decreasing across years
  dose1_cov <- cdf[1:n_yr_test]
  dose2_cov <- cdf[(n_yr_test + 1):(2 * n_yr_test)]
  expect_true(all(diff(dose1_cov) >= 0))
  expect_true(all(diff(dose2_cov) >= 0))

  # Dose 2 coverage cannot exceed dose 1 coverage at any life year
  expect_true(all(dose2_cov <= dose1_cov + 1e-12))
})
