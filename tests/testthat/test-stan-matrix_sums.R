skip_if_not_installed("rstan")
# ' "functions/matrix_sums.stan" defines
# ' `row_vector colsum(matrix obj)` and `vector rowsum(matrix obj)`
# ' which compute column and row marginal sums of a matrix.

target <- "functions/matrix_sums.stan"

skip_if_stan_unchanged(target)

model_matrix_sums <- sprintf(
  "
functions {
  #include %s
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
  row_vector[C] out_colsum = colsum(M);
  vector[R] out_rowsum = rowsum(M);
}
",
  target
) |>
  compile_stan_harness()

test_that("colsum and rowsum compute column and row marginal sums accurately", {
  m_mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)

  out_colsum <- run_stan_harness(
    model_matrix_sums,
    data = list(
      R = nrow(m_mat),
      C = ncol(m_mat),
      M = m_mat
    ),
    out_colsum
  )
  out_rowsum <- run_stan_harness(
    model_matrix_sums,
    data = list(
      R = nrow(m_mat),
      C = ncol(m_mat),
      M = m_mat
    ),
    out_rowsum
  )

  expect_equal(out_colsum, colSums(m_mat))
  expect_equal(out_rowsum, rowSums(m_mat))
})
