skip_if_not_installed("rstan")
# ' "functions/element_mult_expand.stan" defines
# ' `matrix element_mult_expand(vector colv, row_vector rowv)`
# ' which creates a matrix where each column is multiplied by the corresponding
# ' row entry (outer product between a column vector and row vector).

target <- "functions/element_mult_expand.stan"

skip_if_stan_unchanged(target)

model_expand <- sprintf(
  "
functions {
  #include %s
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
  matrix[N_col, N_row] out_expand = element_mult_expand(colv, rowv);
}
",
  target
) |>
  compile_stan_harness()

test_that("element_mult_expand computes outer product matrix between vector and row_vector", {
  colv <- c(2.0, 3.0)
  rowv <- c(1.0, 4.0, 5.0)

  mat <- run_stan_harness(
    model_expand,
    data = list(
      N_col = length(colv),
      N_row = length(rowv),
      colv = colv,
      rowv = rowv
    ),
    out_expand
  )

  expected <- colv %*% t(rowv)
  expect_equal(mat, expected)
})
