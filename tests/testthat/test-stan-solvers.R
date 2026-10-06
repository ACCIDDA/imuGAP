skip_if_not_installed("rstan")
#' "functions/guess/taylor.stan", "functions/guess/conditioned_guess.stan",
#' "functions/guess/pade_mgf.stan", "functions/solvers/newton.stan",
#' "functions/solvers/haste_halley.stan", and "functions/solvers/builtin_rootfinding.stan"
#' provide exact and approximate solvers for Logit and Probit link aggregation shifts.

targets <- c(
  "functions/clamp_probability.stan",
  "functions/link/logit.stan",
  "functions/link/probit.stan",
  "functions/guess/taylor.stan",
  "functions/guess/pade_mgf.stan",
  "functions/guess/conditioned_guess.stan",
  "functions/solvers/newton.stan",
  "functions/solvers/haste_halley.stan",
  "functions/solvers/builtin_rootfinding.stan"
)

skip_if_stan_unchanged(targets)

model_logit_harness <- sprintf(
  "
functions {
  #include functions/clamp_probability.stan
  #include functions/link/logit.stan
  #include functions/guess/taylor.stan
  #include functions/guess/pade_mgf.stan
  #include functions/guess/conditioned_guess.stan
  #include functions/solvers/newton.stan
  #include functions/solvers/haste_halley.stan
  #include functions/solvers/builtin_rootfinding.stan
}
data {
  int<lower=1> C;
  int<lower=1> K;
  vector[C] p0;
  vector[K] w;
  vector[K] delta;
}
parameters { real dummy; }
model { dummy ~ normal(0, 1); }
generated quantities {
  vector[C] p0_c = clamp_probability(p0);
  vector[C] eta0 = link_fn(p0_c);
  vector[C] z_guess = shift_zero(eta0, p0_c, w, delta);
  vector[C] t4_guess = shift_logit_taylor4(eta0, p0_c, w, delta);
  vector[C] cond_guess = shift_logit_conditioned(eta0, p0_c, w, delta);

  vector[C] logit_taylor2 = shift_logit_taylor2(eta0, p0_c, w, delta);
  vector[C] logit_taylor4 = t4_guess;
  vector[C] logit_conditioned = cond_guess;

  vector[C] logit_newton_naive = solve_shift_newton(z_guess, eta0, p0_c, w, delta, 10, 1e-12);
  vector[C] logit_newton_warm2 = solve_shift_newton(t4_guess, eta0, p0_c, w, delta, 2, 1e-12);
  vector[C] logit_newton_cond2 = solve_shift_newton(cond_guess, eta0, p0_c, w, delta, 2, 1e-12);

  vector[C] logit_halley_naive = solve_shift_halley(z_guess, eta0, p0_c, w, delta, 10, 1e-12);
  vector[C] logit_halley_warm2 = solve_shift_halley(t4_guess, eta0, p0_c, w, delta, 2, 1e-12);
  vector[C] logit_halley_cond2 = solve_shift_halley(cond_guess, eta0, p0_c, w, delta, 2, 1e-12);

  vector[C] logit_builtin_naive = solve_shift_builtin(z_guess, eta0, p0_c, w, delta);
  vector[C] logit_builtin_warm = solve_shift_builtin(t4_guess, eta0, p0_c, w, delta);
  vector[C] logit_builtin_cond = solve_shift_builtin(cond_guess, eta0, p0_c, w, delta);
}
"
) |>
  compile_stan_harness()

model_probit_harness <- sprintf(
  "
functions {
  #include functions/clamp_probability.stan
  #include functions/link/probit.stan
  #include functions/guess/taylor.stan
  #include functions/guess/pade_mgf.stan
  #include functions/guess/conditioned_guess.stan
  #include functions/solvers/newton.stan
  #include functions/solvers/haste_halley.stan
  #include functions/solvers/builtin_rootfinding.stan
}
data {
  int<lower=1> C;
  int<lower=1> K;
  vector[C] p0;
  vector[K] w;
  vector[K] delta;
}
parameters { real dummy; }
model { dummy ~ normal(0, 1); }
generated quantities {
  vector[C] p0_c = clamp_probability(p0);
  vector[C] eta0 = link_fn(p0_c);
  vector[C] z_guess = shift_zero(eta0, p0_c, w, delta);
  vector[C] t4_guess = shift_probit_taylor4(eta0, p0_c, w, delta);
  vector[C] cond_guess = shift_probit_conditioned(eta0, p0_c, w, delta);

  vector[C] probit_taylor2 = shift_probit_taylor2(eta0, p0_c, w, delta);
  vector[C] probit_taylor4 = t4_guess;
  vector[C] probit_conditioned = cond_guess;

  vector[C] probit_newton_naive = solve_shift_newton(z_guess, eta0, p0_c, w, delta, 10, 1e-12);
  vector[C] probit_newton_warm2 = solve_shift_newton(t4_guess, eta0, p0_c, w, delta, 2, 1e-12);
  vector[C] probit_newton_cond2 = solve_shift_newton(cond_guess, eta0, p0_c, w, delta, 2, 1e-12);

  vector[C] probit_halley_naive = solve_shift_halley(z_guess, eta0, p0_c, w, delta, 10, 1e-12);
  vector[C] probit_halley_warm2 = solve_shift_halley(t4_guess, eta0, p0_c, w, delta, 2, 1e-12);
  vector[C] probit_halley_cond2 = solve_shift_halley(cond_guess, eta0, p0_c, w, delta, 2, 1e-12);

  vector[C] probit_builtin_naive = solve_shift_builtin(z_guess, eta0, p0_c, w, delta);
  vector[C] probit_builtin_warm = solve_shift_builtin(t4_guess, eta0, p0_c, w, delta);
  vector[C] probit_builtin_cond = solve_shift_builtin(cond_guess, eta0, p0_c, w, delta);
}
"
) |>
  compile_stan_harness()

test_that("Stan solvers accurately recover exact aggregation shifts across links", {
  p0_vals <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  w_vec <- c(0.2, 0.3, 0.5)
  delta_raw <- c(-0.4, 0.1, 0.1)
  # ensure sum(w * delta) == 0
  delta_vec <- delta_raw - sum(w_vec * delta_raw)

  # Solve exact reference values in R via stats::uniroot
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

  results_logit <- run_stan_harness(
    model_logit_harness,
    data = data_list
  )
  results_probit <- run_stan_harness(
    model_probit_harness,
    data = data_list
  )

  # Check Logit exactness across Newton, Halley, and Built-in solvers
  expect_equal(
    as.numeric(results_logit$logit_halley_naive),
    ref_logit,
    tolerance = 1e-8
  )
  expect_equal(
    as.numeric(results_logit$logit_halley_warm2),
    ref_logit,
    tolerance = 1e-8
  )
  expect_equal(
    as.numeric(results_logit$logit_halley_cond2),
    ref_logit,
    tolerance = 1e-8
  )
  expect_equal(
    as.numeric(results_logit$logit_newton_warm2),
    ref_logit,
    tolerance = 1e-8
  )
  expect_equal(
    as.numeric(results_logit$logit_newton_cond2),
    ref_logit,
    tolerance = 1e-8
  )
  expect_equal(
    as.numeric(results_logit$logit_builtin_naive),
    ref_logit,
    tolerance = 1e-6
  )
  expect_equal(
    as.numeric(results_logit$logit_builtin_warm),
    ref_logit,
    tolerance = 1e-6
  )
  expect_equal(
    as.numeric(results_logit$logit_builtin_cond),
    ref_logit,
    tolerance = 1e-6
  )

  # Taylor & Conditioned approximations track reference closely
  err_t2_logit <- max(abs(as.numeric(results_logit$logit_taylor2) - ref_logit))
  err_t4_logit <- max(abs(as.numeric(results_logit$logit_taylor4) - ref_logit))
  expect_lt(err_t4_logit, err_t2_logit)

  # Check Probit exactness across Newton, Halley, and Built-in solvers
  expect_equal(
    as.numeric(results_probit$probit_halley_naive),
    ref_probit,
    tolerance = 1e-8
  )
  expect_equal(
    as.numeric(results_probit$probit_halley_warm2),
    ref_probit,
    tolerance = 1e-8
  )
  expect_equal(
    as.numeric(results_probit$probit_halley_cond2),
    ref_probit,
    tolerance = 1e-8
  )
  expect_equal(
    as.numeric(results_probit$probit_newton_warm2),
    ref_probit,
    tolerance = 1e-8
  )
  expect_equal(
    as.numeric(results_probit$probit_newton_cond2),
    ref_probit,
    tolerance = 1e-8
  )
  expect_equal(
    as.numeric(results_probit$probit_builtin_naive),
    ref_probit,
    tolerance = 1e-6
  )
  expect_equal(
    as.numeric(results_probit$probit_builtin_warm),
    ref_probit,
    tolerance = 1e-6
  )
  expect_equal(
    as.numeric(results_probit$probit_builtin_cond),
    ref_probit,
    tolerance = 1e-6
  )

  # Probit Taylor & Conditioned approximations track reference closely
  err_t2_probit <- max(abs(
    as.numeric(results_probit$probit_taylor2) - ref_probit
  ))
  err_t4_probit <- max(abs(
    as.numeric(results_probit$probit_taylor4) - ref_probit
  ))
  expect_lt(err_t4_probit, err_t2_probit)
})
