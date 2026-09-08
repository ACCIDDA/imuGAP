skip_if_not_installed("rstan")
# ' "functions/diff.stan" defines
# ' `vector diff(vector obj)` and `row_vector diff(row_vector obj)`
# ' which compute sequential first differences `obj[2:] - obj[:(sz - 1)]`.

target <- "functions/diff.stan"

skip_if_stan_unchanged(target)

model_diff <- sprintf(
  "
functions {
  #include %s
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
  vector[N - 1] out_diff_v = diff(v);
  row_vector[N - 1] out_diff_rv = diff(rv);
}
",
  target
) |>
  compile_stan_harness()

test_that("diff computes sequential differences for vector and row_vector types", {
  v <- c(1.5, 3.5, 7.0, 12.0)

  out_diff_v <- run_stan_harness(
    model_diff,
    data = list(N = length(v), v = v, rv = v),
    out_diff_v
  )
  out_diff_rv <- run_stan_harness(
    model_diff,
    data = list(N = length(v), v = v, rv = v),
    out_diff_rv
  )

  expected <- diff(v)
  expect_equal(out_diff_v, expected)
  expect_equal(out_diff_rv, expected)
})
