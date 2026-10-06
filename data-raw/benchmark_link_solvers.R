#' Comprehensive Accuracy and Speed Benchmark for Link Aggregation Solvers
#' Evaluates Logit vs. Probit across:
#'  1. Exact numerical reference (uniroot)
#'  2. Taylor 2nd-order moment expansion (m2)
#'  3. Taylor 4th-order moment expansion (m2, m3, m4)
#'  4. HASTE Halley with Taylor4 warm-start (1 step)
#'  5. HASTE Halley with Taylor4 warm-start (2 steps)
#'  6. Naive Halley with 0 start (10 steps)
#'  7. Stan built-in algebra_solver with Taylor4 warm-start
#'  8. Stan built-in algebra_solver with naive 0 start

suppressPackageStartupMessages({
  library(microbenchmark)
  library(data.table)
  library(rstan)
  library(imuGAP)
})

# Load Stan solver test harness
source("tests/testthat/helper-stan-test.R")

targets <- c(
  "functions/clamp_probability.stan",
  "functions/link/logit.stan",
  "functions/link/probit.stan",
  "functions/guess/taylor.stan",
  "functions/guess/conditioned_guess.stan",
  "functions/solvers/haste_halley.stan",
  "functions/solvers/builtin_rootfinding.stan"
)

model_logit_harness <- sprintf(
  "
functions {
  #include functions/clamp_probability.stan
  #include functions/link/logit.stan
  #include functions/guess/taylor.stan
  #include functions/guess/conditioned_guess.stan
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
parameters {
  real dummy;
}
model {
  dummy ~ normal(0, 1);
}
generated quantities {
  vector[C] p0_c = clamp_probability(p0);
  vector[C] eta0 = link_fn(p0_c);
  vector[C] z_guess = shift_zero(eta0, p0_c, w, delta);
  vector[C] t4_guess = shift_logit_taylor4(eta0, p0_c, w, delta);

  vector[C] logit_taylor2 = shift_logit_taylor2(eta0, p0_c, w, delta);
  vector[C] logit_taylor4 = t4_guess;
  vector[C] logit_halley_naive = solve_shift_halley(z_guess, eta0, p0_c, w, delta, 10, 1e-12);
  vector[C] logit_halley_warm1 = solve_shift_halley(t4_guess, eta0, p0_c, w, delta, 1, 1e-12);
  vector[C] logit_halley_warm2 = solve_shift_halley(t4_guess, eta0, p0_c, w, delta, 2, 1e-12);
  vector[C] logit_builtin_naive = solve_shift_builtin(z_guess, eta0, p0_c, w, delta);
  vector[C] logit_builtin_warm = solve_shift_builtin(t4_guess, eta0, p0_c, w, delta);
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
  #include functions/guess/conditioned_guess.stan
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
parameters {
  real dummy;
}
model {
  dummy ~ normal(0, 1);
}
generated quantities {
  vector[C] p0_c = clamp_probability(p0);
  vector[C] eta0 = link_fn(p0_c);
  vector[C] z_guess = shift_zero(eta0, p0_c, w, delta);
  vector[C] t4_guess = shift_probit_taylor4(eta0, p0_c, w, delta);

  vector[C] probit_taylor2 = shift_probit_taylor2(eta0, p0_c, w, delta);
  vector[C] probit_taylor4 = t4_guess;
  vector[C] probit_halley_naive = solve_shift_halley(z_guess, eta0, p0_c, w, delta, 10, 1e-12);
  vector[C] probit_halley_warm1 = solve_shift_halley(t4_guess, eta0, p0_c, w, delta, 1, 1e-12);
  vector[C] probit_halley_warm2 = solve_shift_halley(t4_guess, eta0, p0_c, w, delta, 2, 1e-12);
  vector[C] probit_builtin_naive = solve_shift_builtin(z_guess, eta0, p0_c, w, delta);
  vector[C] probit_builtin_warm = solve_shift_builtin(t4_guess, eta0, p0_c, w, delta);
}
"
) |>
  compile_stan_harness()

# Pure R reference exact solver
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
        interval = c(-8, 8),
        tol = 1e-12
      )$root
    },
    numeric(1)
  )
}

# Define factorial test conditions
p0_grid <- list(
  central = c(0.4, 0.5, 0.6),
  moderate = c(0.15, 0.5, 0.85),
  extreme = c(0.01, 0.05, 0.95, 0.99)
)

sigma_grid <- c(low = 0.2, med = 0.6, high = 1.2)
K_grid <- c(small = 3L, med = 10L, large = 30L)

results_list <- list()

set.seed(42)

for (p_name in names(p0_grid)) {
  p0_vec <- p0_grid[[p_name]]
  C <- length(p0_vec)

  for (s_name in names(sigma_grid)) {
    sig <- sigma_grid[[s_name]]

    for (k_name in names(K_grid)) {
      K <- K_grid[[k_name]]

      w_raw <- stats::runif(K, 0.5, 2.0)
      w <- w_raw / sum(w_raw)
      delta_raw <- stats::rnorm(K, mean = 0, sd = sig)
      delta <- delta_raw - sum(w * delta_raw)

      ref_logit <- solve_exact_r(p0_vec, w, delta, "logit")
      ref_probit <- solve_exact_r(p0_vec, w, delta, "probit")

      dat <- list(
        C = C,
        K = K,
        p0 = p0_vec,
        w = w,
        delta = delta
      )

      sol_log <- run_stan_harness(model_logit_harness, data = dat)
      sol_prob <- run_stan_harness(model_probit_harness, data = dat)

      # Extract errors
      calc_err <- function(res, ref) {
        max(abs(as.numeric(res) - ref))
      }

      results_list[[length(results_list) + 1L]] <- data.table(
        p0_regime = p_name,
        sigma_regime = s_name,
        sigma_val = sig,
        K_strata = K,
        # Logit errors
        logit_err_t2 = calc_err(sol_log$logit_taylor2, ref_logit),
        logit_err_t4 = calc_err(sol_log$logit_taylor4, ref_logit),
        logit_err_halley_warm1 = calc_err(
          sol_log$logit_halley_warm1,
          ref_logit
        ),
        logit_err_halley_warm2 = calc_err(
          sol_log$logit_halley_warm2,
          ref_logit
        ),
        logit_err_halley_naive = calc_err(
          sol_log$logit_halley_naive,
          ref_logit
        ),
        logit_err_builtin_warm = calc_err(
          sol_log$logit_builtin_warm,
          ref_logit
        ),
        logit_err_builtin_naive = calc_err(
          sol_log$logit_builtin_naive,
          ref_logit
        ),
        # Probit errors
        probit_err_t2 = calc_err(sol_prob$probit_taylor2, ref_probit),
        probit_err_t4 = calc_err(sol_prob$probit_taylor4, ref_probit),
        probit_err_halley_warm1 = calc_err(
          sol_prob$probit_halley_warm1,
          ref_probit
        ),
        probit_err_halley_warm2 = calc_err(
          sol_prob$probit_halley_warm2,
          ref_probit
        ),
        probit_err_halley_naive = calc_err(
          sol_prob$probit_halley_naive,
          ref_probit
        ),
        probit_err_builtin_warm = calc_err(
          sol_prob$probit_builtin_warm,
          ref_probit
        ),
        probit_err_builtin_naive = calc_err(
          sol_prob$probit_builtin_naive,
          ref_probit
        )
      )
    }
  }
}

benchmark_dt <- rbindlist(results_list)
print(benchmark_dt)
saveRDS(benchmark_dt, "data-raw/benchmark_solver_results.rds")
