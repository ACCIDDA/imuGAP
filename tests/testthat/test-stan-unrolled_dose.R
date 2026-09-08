skip_if_not_installed("rstan")
# ' "functions/unrolled_dose_static_lambda.stan" defines
# ' `vector unrolled_dose(int n_yr, int n_doses, matrix dose_sched, ...)`
# ' which computes cumulative vaccination coverage across years and doses
# ' subject to dose eligibility schedules and precedence constraints.

target <- "functions/unrolled_dose_static_lambda.stan"

skip_if_stan_unchanged(c(
  "functions/diff.stan",
  target
))

model_unrolled <- sprintf(
  "
functions {
  #include functions/diff.stan
  #include %s
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
  vector[n_doses * n_yr] out_unrolled_dose = unrolled_dose(
    n_yr, n_doses, dose_sched, lambda_raw, epsilon_p
  );
}
",
  target
) |>
  compile_stan_harness()

test_that("unrolled_dose computes cumulative dose coverage correctly with dose constraints", {
  dose_sched <- matrix(
    c(1.0, 1.0, 1.0, 0.5, 0.5, 0.5),
    nrow = 3L,
    ncol = 2L
  )
  n_yr <- nrow(dose_sched)
  n_doses <- ncol(dose_sched)
  lambda_raw <- c(log(0.5), log(0.3))

  cdf <- run_stan_harness(
    model_unrolled,
    data = list(
      n_yr = n_yr,
      n_doses = n_doses,
      dose_sched = dose_sched,
      lambda_raw = lambda_raw,
      epsilon_p = 1e-9
    ),
    out_unrolled_dose
  )

  expect_length(cdf, n_yr * n_doses)
  expect_true(all(cdf >= 0 & cdf <= 1))

  # Monotonicity across years
  dose1_cov <- cdf[1:n_yr]
  dose2_cov <- cdf[(n_yr + 1):(2 * n_yr)]
  expect_true(all(diff(dose1_cov) >= 0))
  expect_true(all(diff(dose2_cov) >= 0))

  # Precedence constraint: dose 2 <= dose 1
  expect_true(all(dose2_cov <= dose1_cov + 1e-12))
})
