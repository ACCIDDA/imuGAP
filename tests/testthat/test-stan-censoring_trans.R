skip_if_not_installed("rstan")
# ' "transformed_data/censoring.stan" defines
# ' `array[n_obs] int y_obs_trans`
# ' which transforms count observation bounds by shifting censored indices by -1.

target <- "transformed_data/censoring.stan"

skip_if_stan_unchanged(target)

model_censoring_trans <- sprintf(
  "
data {
  int n_obs;
  array[n_obs] int y_obs;
}
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
  array[n_obs] int out_y_trans = y_obs_trans;
}
",
  target
) |>
  compile_stan_harness()

test_that("censoring.stan shifts count observation bounds", {
  y_obs_test <- c(10L, 25L, 0L)
  out_y_trans <- run_stan_harness(
    model_censoring_trans,
    data = list(n_obs = length(y_obs_test), y_obs = y_obs_test),
    out_y_trans
  )

  expect_equal(out_y_trans, y_obs_test - 1L)
})
