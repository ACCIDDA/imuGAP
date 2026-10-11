skip_if_not_installed("rstan")

target_guess <- "functions/guess/zero.stan"
target_solver <- "functions/solvers/direct.stan"

skip_if_stan_unchanged(c(target_guess, target_solver))

model_solvers <- sprintf(
  "
functions {
  #include %s
  #include %s
}
data {
  int n_cohort;
  int K;
  vector[n_cohort] eta0;
  vector[n_cohort] p0;
  vector[K] w;
  vector[K] delta;
}
parameters {
  real dummy;
}
model {
  dummy ~ normal(0, 1);
}
generated quantities {
  vector[n_cohort] out_guess = guess_subpop_shift(eta0, p0, w, delta);
  vector[n_cohort] out_direct = solve_subpop_shift(eta0, p0, w, delta);
}
",
  target_guess,
  target_solver
) |>
  compile_stan_harness()

test_that("zero guesser and direct solver execute and return baseline zero shifts", {
  n_cohort <- 5L
  k_subpop <- 4L
  eta0 <- rnorm(n_cohort)
  p0 <- stats::plogis(eta0)
  w <- c(0.1, 0.2, 0.3, 0.4)
  delta <- rnorm(k_subpop)

  data_list <- list(
    n_cohort = n_cohort,
    K = k_subpop,
    eta0 = eta0,
    p0 = p0,
    w = w,
    delta = delta
  )

  guess_res <- run_stan_harness(
    model_solvers,
    data = data_list,
    out_guess
  )

  direct_res <- run_stan_harness(
    model_solvers,
    data = data_list,
    out_direct
  )

  expect_equal(guess_res, rep(0.0, n_cohort), tolerance = 1e-9)
  expect_equal(direct_res, rep(0.0, n_cohort), tolerance = 1e-9)
})
