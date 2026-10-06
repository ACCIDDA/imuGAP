skip_if_not_installed("rstan")
#' "functions/clamp_probability.stan" defines
#' `real clamp_probability(real p)` and `vector clamp_probability(vector p)`
#' which clamp probabilities to [1e-12, 1 - 1e-12] to prevent overflow/underflow.

target <- "functions/clamp_probability.stan"

skip_if_stan_unchanged(target)

model_clamp <- sprintf(
  "
functions {
  #include %s
}
data {
  int N;
  vector[N] p_in;
  real p_scalar;
}
parameters {
  real dummy;
}
model {
  dummy ~ normal(0, 1);
}
generated quantities {
  vector[N] p_clamped_vec = clamp_probability(p_in);
  real p_clamped_scalar = clamp_probability(p_scalar);
}
",
  target
) |>
  compile_stan_harness()

test_that("clamp_probability clamps probabilities correctly for scalars and vectors", {
  p_in <- c(-0.5, 0.0, 1e-15, 0.5, 1.0 - 1e-15, 1.0, 1.5)
  res_vec <- run_stan_harness(
    model_clamp,
    data = list(N = length(p_in), p_in = p_in, p_scalar = 0.0),
    p_clamped_vec
  )

  expect_equal(res_vec[1], 1e-12)
  expect_equal(res_vec[2], 1e-12)
  expect_equal(res_vec[3], 1e-12)
  expect_equal(res_vec[4], 0.5)
  expect_equal(res_vec[5], 1.0 - 1e-12)
  expect_equal(res_vec[6], 1.0 - 1e-12)
  expect_equal(res_vec[7], 1.0 - 1e-12)

  res_scalar_0 <- run_stan_harness(
    model_clamp,
    data = list(N = length(p_in), p_in = p_in, p_scalar = 0.0),
    p_clamped_scalar
  )
  expect_equal(res_scalar_0, 1e-12)

  res_scalar_1 <- run_stan_harness(
    model_clamp,
    data = list(N = length(p_in), p_in = p_in, p_scalar = 1.0),
    p_clamped_scalar
  )
  expect_equal(res_scalar_1, 1.0 - 1e-12)
})
