skip_if_not_installed("rstan")
#' "functions/guess/taylor2_logit.stan", "functions/guess/taylor4_logit.stan",
#' "functions/guess/conditioned_logit.stan", "functions/guess/taylor2_probit.stan",
#' "functions/guess/taylor4_probit.stan", "functions/guess/conditioned_probit.stan",
#' "functions/solvers/direct.stan", "functions/solvers/newton2.stan",
#' "functions/solvers/halley2.stan", "functions/solvers/halley10.stan",
#' and "functions/solvers/builtin.stan" provide exact and approximate solvers.

targets <- c(
  "functions/link/logit.stan",
  "functions/link/probit.stan",
  "functions/guess/zero.stan",
  "functions/guess/asymptotic.stan",
  "functions/guess/asymptotic_logit.stan",
  "functions/guess/asymptotic_probit.stan",
  "functions/guess/mgf_logit.stan",
  "functions/guess/pade.stan",
  "functions/guess/pade_probit.stan",
  "functions/guess/taylor_logit.stan",
  "functions/guess/taylor_probit.stan",
  "functions/guess/taylor2_logit.stan",
  "functions/guess/taylor4_logit.stan",
  "functions/guess/conditioned_logit.stan",
  "functions/guess/taylor2_probit.stan",
  "functions/guess/taylor4_probit.stan",
  "functions/guess/conditioned_probit.stan",
  "functions/solvers/direct.stan",
  "functions/solvers/newton2.stan",
  "functions/solvers/halley2.stan",
  "functions/solvers/halley10.stan",
  "functions/solvers/builtin.stan"
)

skip_if_stan_unchanged(targets)

model_logit_harness <- sprintf(
  "
functions {
  #include functions/link/logit.stan
  #include functions/solvers/haste_halley.stan
  #include functions/solvers/newton.stan
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
  vector[C] eta0 = link_fn(p0);
  vector[C] z_guess = rep_vector(0.0, C);

  // Exact solvers from zero guess
  vector[C] logit_halley_naive = solve_shift_halley(z_guess, eta0, p0, w, delta, 10);
  vector[C] logit_builtin_naive = solve_shift_builtin(z_guess, eta0, p0, w, delta);
}
"
) |>
  compile_stan_harness()

model_probit_harness <- sprintf(
  "
functions {
  #include functions/link/probit.stan
  #include functions/solvers/haste_halley.stan
  #include functions/solvers/newton.stan
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
  vector[C] eta0 = link_fn(p0);
  vector[C] z_guess = rep_vector(0.0, C);

  // Exact solvers from zero guess
  vector[C] probit_halley_naive = solve_shift_halley(z_guess, eta0, p0, w, delta, 10);
  vector[C] probit_builtin_naive = solve_shift_builtin(z_guess, eta0, p0, w, delta);
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

  # Check Logit exactness
  expect_equal(
    as.numeric(results_logit$logit_halley_naive),
    ref_logit,
    tolerance = 1e-8
  )
  expect_equal(
    as.numeric(results_logit$logit_builtin_naive),
    ref_logit,
    tolerance = 1e-6
  )

  # Check Probit exactness
  expect_equal(
    as.numeric(results_probit$probit_halley_naive),
    ref_probit,
    tolerance = 1e-8
  )
  expect_equal(
    as.numeric(results_probit$probit_builtin_naive),
    ref_probit,
    tolerance = 1e-6
  )
})
