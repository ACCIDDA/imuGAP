skip_if_not_installed("rstan")
# ' "transformed_data/epsilon.stan" defines
# ' `real epsilon_p = 1e-10`
# ' which sets a numerical stability tolerance constant used across models.

target <- "transformed_data/epsilon.stan"

skip_if_stan_unchanged(target)

model_epsilon <- sprintf(
  "
transformed data {
  #include %s
}
parameters {
  real dummy;
}
model {
  dummy ~ normal(0, 1);
}
generated quantities {
  real out_eps = epsilon_p;
}
",
  target
) |>
  compile_stan_harness()

test_that("epsilon.stan defines numerical stability precision constant", {
  out_eps <- run_stan_harness(model_epsilon, data = list(), out_eps)
  expect_equal(out_eps, 1e-10)
})
