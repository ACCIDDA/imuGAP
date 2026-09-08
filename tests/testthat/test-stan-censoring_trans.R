skip_if_not_installed("rstan")
# ' "transformed_data/censoring.stan" defines
# ' `array[n_obs_right] int y_fail_right`
# ' which transforms count observations into failures (y_smp_right - y_obs_right).

target <- "transformed_data/censoring.stan"

skip_if_stan_unchanged(target)

model_censoring_trans <- sprintf(
  "
data {
  int<lower=0> n_obs_right;
  array[n_obs_right] int y_obs_right;
  array[n_obs_right] int y_smp_right;
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
  array[n_obs_right] int out_y_fail = y_fail_right;
}
",
  target
) |>
  compile_stan_harness()

test_that("censoring.stan computes right-censored failure counts", {
  y_obs_test <- c(10L, 25L, 0L)
  y_smp_test <- c(20L, 30L, 5L)
  out_y_fail <- run_stan_harness(
    model_censoring_trans,
    data = list(
      n_obs_right = length(y_obs_test),
      y_obs_right = y_obs_test,
      y_smp_right = y_smp_test
    ),
    out_y_fail
  )

  expect_equal(out_y_fail, y_smp_test - y_obs_test)
})
