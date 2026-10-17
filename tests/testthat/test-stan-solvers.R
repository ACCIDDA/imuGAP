skip_if_not_installed("rstan")

targets <- c(
  "functions/link/logit.stan",
  "functions/link/probit.stan",
  "functions/guess/zero.stan",
  "functions/solvers/direct.stan",
  "functions/solvers/haste_halley.stan",
  "functions/solvers/newton.stan",
  "functions/solvers/builtin_rootfinding.stan",
  "functions/solvers/halley2.stan",
  "functions/solvers/halley10.stan",
  "functions/solvers/newton2.stan",
  "functions/solvers/builtin.stan",
  "functions/solvers/dispatch.stan"
)

skip_if_stan_unchanged(targets)

model_solvers <- sprintf(
  "
functions {
  #include functions/guess/zero.stan
  #include functions/solvers/direct.stan
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
"
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

model_dispatch_logit <- sprintf(
  "
functions {
  #include functions/link/logit.stan
  #include functions/guess/zero.stan
  #include functions/solvers/dispatch.stan
}
data {
  int<lower=1> C;
  int<lower=1> K;
  vector[C] p0;
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
  vector[C] eta0 = link_fn(p0);
  vector[C] sol_direct = solve_subpop_shift(eta0, p0, w, delta, 1);
  vector[C] sol_halley2 = solve_subpop_shift(eta0, p0, w, delta, 2);
  vector[C] sol_newton2 = solve_subpop_shift(eta0, p0, w, delta, 3);
  vector[C] sol_builtin = solve_subpop_shift(eta0, p0, w, delta, 4);
  vector[C] sol_halley10 = solve_subpop_shift(eta0, p0, w, delta, 5);
}
"
) |>
  compile_stan_harness()

model_dispatch_probit <- sprintf(
  "
functions {
  #include functions/link/probit.stan
  #include functions/guess/zero.stan
  #include functions/solvers/dispatch.stan
}
data {
  int<lower=1> C;
  int<lower=1> K;
  vector[C] p0;
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
  vector[C] eta0 = link_fn(p0);
  vector[C] sol_direct = solve_subpop_shift(eta0, p0, w, delta, 1);
  vector[C] sol_halley2 = solve_subpop_shift(eta0, p0, w, delta, 2);
  vector[C] sol_newton2 = solve_subpop_shift(eta0, p0, w, delta, 3);
  vector[C] sol_builtin = solve_subpop_shift(eta0, p0, w, delta, 4);
  vector[C] sol_halley10 = solve_subpop_shift(eta0, p0, w, delta, 5);
}
"
) |>
  compile_stan_harness()

test_that("Stan solvers accurately recover exact aggregation shifts across links", {
  p0_vals <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  w_vec <- c(0.2, 0.3, 0.5)
  delta_raw <- c(-0.4, 0.1, 0.1)
  delta_vec <- delta_raw - sum(w_vec * delta_raw)

  solve_exact_r <- function(p0, w, delta, link = c("logit", "probit")) {
    link <- match.arg(link)
    inv_link <- if (link == "logit") stats::plogis else stats::pnorm
    link_fn <- if (link == "logit") stats::qlogis else stats::qnorm
    vapply(
      p0,
      function(p) {
        eta0 <- link_fn(p)
        stats::uniroot(
          function(mu) sum(w * inv_link(eta0 + mu + delta)) - p,
          interval = c(-5, 5),
          tol = 1e-12
        )$root
      },
      numeric(1)
    )
  }

  ref_logit <- solve_exact_r(p0_vals, w_vec, delta_vec, "logit")
  ref_probit <- solve_exact_r(p0_vals, w_vec, delta_vec, "probit")

  data_list <- list(
    C = length(p0_vals),
    K = length(w_vec),
    p0 = p0_vals,
    w = w_vec,
    delta = delta_vec
  )

  res_logit <- run_stan_harness(model_dispatch_logit, data = data_list)
  res_probit <- run_stan_harness(model_dispatch_probit, data = data_list)

  # Check direct solver returns 0 (baseline)
  expect_equal(
    as.numeric(res_logit$sol_direct),
    rep(0, length(p0_vals)),
    tolerance = 1e-9
  )
  expect_equal(
    as.numeric(res_probit$sol_direct),
    rep(0, length(p0_vals)),
    tolerance = 1e-9
  )

  # Check high-precision Halley (10 steps) matches uniroot
  expect_equal(as.numeric(res_logit$sol_halley10), ref_logit, tolerance = 1e-8)
  expect_equal(
    as.numeric(res_probit$sol_halley10),
    ref_probit,
    tolerance = 1e-8
  )

  # Check Stan algebraic solver matches uniroot
  expect_equal(as.numeric(res_logit$sol_builtin), ref_logit, tolerance = 1e-6)
  expect_equal(as.numeric(res_probit$sol_builtin), ref_probit, tolerance = 1e-6)

  # Check 2-step Halley and Newton approximate uniroot closely
  expect_equal(as.numeric(res_logit$sol_halley2), ref_logit, tolerance = 1e-3)
  expect_equal(as.numeric(res_probit$sol_halley2), ref_probit, tolerance = 1e-3)
  expect_equal(as.numeric(res_logit$sol_newton2), ref_logit, tolerance = 1e-2)
  expect_equal(as.numeric(res_probit$sol_newton2), ref_probit, tolerance = 1e-2)
})
