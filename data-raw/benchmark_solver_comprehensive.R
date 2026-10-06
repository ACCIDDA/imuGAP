#' Comprehensive Benchmark of Link Aggregation Solvers
#' Evaluates error, residual, and runtime metrics for Logit & Probit across:
#'  - Closed-form: Taylor2, Taylor4, Asymptotic, Conditioned, MGF (Logit), Pade11 (Probit)
#'  - Newton-Raphson: Warm Taylor4, Warm Conditioned, Warm MGF/Pade11, Naive 10-step
#'  - Halley: Warm Taylor4, Warm Conditioned, Warm MGF/Pade11, Naive 10-step
#'  - Stan Built-in algebra_solver: Naive 0, Warm Taylor4, Warm Conditioned, Warm MGF/Pade11

suppressPackageStartupMessages({
  library(microbenchmark)
  library(data.table)
  library(rstan)
  library(imuGAP)
})

source("tests/testthat/helper-stan-test.R")

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

cat("Compiling comprehensive Stan solver evaluation harnesses...\n")
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
parameters {
  real dummy;
}
model {
  dummy ~ normal(0, 1);
}
generated quantities {
  // Initial Guesses / Warm Starts
  vector[C] p0_c = clamp_probability(p0);
  vector[C] eta0 = link_fn(p0_c);
  vector[C] z_guess = shift_zero(eta0, p0_c, w, delta);
  vector[C] logit_t4_guess = shift_logit_taylor4(eta0, p0_c, w, delta);
  vector[C] logit_cond_guess = shift_logit_conditioned(eta0, p0_c, w, delta);
  vector[C] logit_mgf_guess = shift_logit_mgf(eta0, p0_c, w, delta);

  // Logit closed-form
  vector[C] logit_taylor2 = shift_logit_taylor2(eta0, p0_c, w, delta);
  vector[C] logit_taylor4 = logit_t4_guess;
  vector[C] logit_asymp = shift_logit_asymptotic(eta0, p0_c, w, delta);
  vector[C] logit_cond = logit_cond_guess;
  vector[C] logit_mgf = logit_mgf_guess;

  // Logit Newton
  vector[C] logit_newton_warm1 = solve_shift_newton(logit_t4_guess, eta0, p0_c, w, delta, 1, 1e-12);
  vector[C] logit_newton_warm2 = solve_shift_newton(logit_t4_guess, eta0, p0_c, w, delta, 2, 1e-12);
  vector[C] logit_newton_cond1 = solve_shift_newton(logit_cond_guess, eta0, p0_c, w, delta, 1, 1e-12);
  vector[C] logit_newton_cond2 = solve_shift_newton(logit_cond_guess, eta0, p0_c, w, delta, 2, 1e-12);
  vector[C] logit_newton_mgf1 = solve_shift_newton(logit_mgf_guess, eta0, p0_c, w, delta, 1, 1e-12);
  vector[C] logit_newton_mgf2 = solve_shift_newton(logit_mgf_guess, eta0, p0_c, w, delta, 2, 1e-12);
  vector[C] logit_newton_naive = solve_shift_newton(z_guess, eta0, p0_c, w, delta, 10, 1e-12);

  // Logit Halley
  vector[C] logit_halley_warm1 = solve_shift_halley(logit_t4_guess, eta0, p0_c, w, delta, 1, 1e-12);
  vector[C] logit_halley_warm2 = solve_shift_halley(logit_t4_guess, eta0, p0_c, w, delta, 2, 1e-12);
  vector[C] logit_halley_cond1 = solve_shift_halley(logit_cond_guess, eta0, p0_c, w, delta, 1, 1e-12);
  vector[C] logit_halley_cond2 = solve_shift_halley(logit_cond_guess, eta0, p0_c, w, delta, 2, 1e-12);
  vector[C] logit_halley_mgf1 = solve_shift_halley(logit_mgf_guess, eta0, p0_c, w, delta, 1, 1e-12);
  vector[C] logit_halley_mgf2 = solve_shift_halley(logit_mgf_guess, eta0, p0_c, w, delta, 2, 1e-12);
  vector[C] logit_halley_mgf_max10 = solve_shift_halley(logit_mgf_guess, eta0, p0_c, w, delta, 10, 1e-12);
  vector[C] logit_halley_cond_max10 = solve_shift_halley(logit_cond_guess, eta0, p0_c, w, delta, 10, 1e-12);
  vector[C] logit_halley_naive = solve_shift_halley(z_guess, eta0, p0_c, w, delta, 10, 1e-12);

  // Logit Builtin
  vector[C] logit_builtin_naive = solve_shift_builtin(z_guess, eta0, p0_c, w, delta);
  vector[C] logit_builtin_warm = solve_shift_builtin(logit_t4_guess, eta0, p0_c, w, delta);
  vector[C] logit_builtin_cond = solve_shift_builtin(logit_cond_guess, eta0, p0_c, w, delta);
  vector[C] logit_builtin_mgf = solve_shift_builtin(logit_mgf_guess, eta0, p0_c, w, delta);
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
parameters {
  real dummy;
}
model {
  dummy ~ normal(0, 1);
}
generated quantities {
  // Initial Guesses / Warm Starts
  vector[C] p0_c = clamp_probability(p0);
  vector[C] eta0 = link_fn(p0_c);
  vector[C] z_guess = shift_zero(eta0, p0_c, w, delta);
  vector[C] probit_t4_guess = shift_probit_taylor4(eta0, p0_c, w, delta);
  vector[C] probit_cond_guess = shift_probit_conditioned(eta0, p0_c, w, delta);
  vector[C] probit_pade_guess = shift_probit_pade11(eta0, p0_c, w, delta);

  // Probit closed-form
  vector[C] probit_taylor2 = shift_probit_taylor2(eta0, p0_c, w, delta);
  vector[C] probit_taylor4 = probit_t4_guess;
  vector[C] probit_asymp = shift_probit_asymptotic(eta0, p0_c, w, delta);
  vector[C] probit_cond = probit_cond_guess;
  vector[C] probit_pade11 = probit_pade_guess;

  // Probit Newton
  vector[C] probit_newton_warm1 = solve_shift_newton(probit_t4_guess, eta0, p0_c, w, delta, 1, 1e-12);
  vector[C] probit_newton_warm2 = solve_shift_newton(probit_t4_guess, eta0, p0_c, w, delta, 2, 1e-12);
  vector[C] probit_newton_cond1 = solve_shift_newton(probit_cond_guess, eta0, p0_c, w, delta, 1, 1e-12);
  vector[C] probit_newton_cond2 = solve_shift_newton(probit_cond_guess, eta0, p0_c, w, delta, 2, 1e-12);
  vector[C] probit_newton_pade1 = solve_shift_newton(probit_pade_guess, eta0, p0_c, w, delta, 1, 1e-12);
  vector[C] probit_newton_pade2 = solve_shift_newton(probit_pade_guess, eta0, p0_c, w, delta, 2, 1e-12);
  vector[C] probit_newton_naive = solve_shift_newton(z_guess, eta0, p0_c, w, delta, 10, 1e-12);

  // Probit Halley
  vector[C] probit_halley_warm1 = solve_shift_halley(probit_t4_guess, eta0, p0_c, w, delta, 1, 1e-12);
  vector[C] probit_halley_warm2 = solve_shift_halley(probit_t4_guess, eta0, p0_c, w, delta, 2, 1e-12);
  vector[C] probit_halley_cond1 = solve_shift_halley(probit_cond_guess, eta0, p0_c, w, delta, 1, 1e-12);
  vector[C] probit_halley_cond2 = solve_shift_halley(probit_cond_guess, eta0, p0_c, w, delta, 2, 1e-12);
  vector[C] probit_halley_pade1 = solve_shift_halley(probit_pade_guess, eta0, p0_c, w, delta, 1, 1e-12);
  vector[C] probit_halley_pade2 = solve_shift_halley(probit_pade_guess, eta0, p0_c, w, delta, 2, 1e-12);
  vector[C] probit_halley_pade_max10 = solve_shift_halley(probit_pade_guess, eta0, p0_c, w, delta, 10, 1e-12);
  vector[C] probit_halley_cond_max10 = solve_shift_halley(probit_cond_guess, eta0, p0_c, w, delta, 10, 1e-12);
  vector[C] probit_halley_naive = solve_shift_halley(z_guess, eta0, p0_c, w, delta, 10, 1e-12);

  // Probit Builtin
  vector[C] probit_builtin_naive = solve_shift_builtin(z_guess, eta0, p0_c, w, delta);
  vector[C] probit_builtin_warm = solve_shift_builtin(probit_t4_guess, eta0, p0_c, w, delta);
  vector[C] probit_builtin_cond = solve_shift_builtin(probit_cond_guess, eta0, p0_c, w, delta);
  vector[C] probit_builtin_pade = solve_shift_builtin(probit_pade_guess, eta0, p0_c, w, delta);
}
"
) |>
  compile_stan_harness()

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
        interval = c(-10, 10),
        tol = 1e-14
      )$root
    },
    numeric(1)
  )
}

calc_residual <- function(p0, w, delta, mu, link = c("logit", "probit")) {
  link <- match.arg(link)
  inv_link <- if (link == "logit") stats::plogis else stats::pnorm
  link_fn <- if (link == "logit") stats::qlogis else stats::qnorm
  vapply(
    seq_along(p0),
    function(i) {
      p <- p0[i]
      m <- mu[i]
      eta0 <- link_fn(p)
      abs(sum(w * inv_link(eta0 + m + delta)) - p)
    },
    numeric(1)
  )
}

p0_regimes <- list(
  central = c(0.4, 0.5, 0.6),
  moderate = c(0.15, 0.5, 0.85),
  edge = c(0.05, 0.90, 0.95),
  extreme = c(0.005, 0.01, 0.99, 0.995)
)

sigma_vals <- c(0.1, 0.3, 0.6, 1.0, 1.5, 2.0)
K_vals <- c(2L, 5L, 10L, 25L, 50L)

results <- list()
set.seed(12345)

cat("Starting factorial solver evaluation across regimes...\n")
case_idx <- 0L

for (p_reg in names(p0_regimes)) {
  p0_vec <- p0_regimes[[p_reg]]
  C <- length(p0_vec)

  for (sig in sigma_vals) {
    for (K in K_vals) {
      case_idx <- case_idx + 1L

      w_raw <- stats::runif(K, 0.5, 2.0)
      w <- w_raw / sum(w_raw)
      delta_raw <- stats::rnorm(K, mean = 0, sd = sig)
      delta <- delta_raw - sum(w * delta_raw)
      m2 <- sum(w * delta^2)
      m3 <- sum(w * delta^3)
      m4 <- sum(w * delta^4)

      ref_logit <- solve_exact_r(p0_vec, w, delta, "logit")
      ref_probit <- solve_exact_r(p0_vec, w, delta, "probit")

      dat <- list(
        C = C,
        K = K,
        p0 = p0_vec,
        w = w,
        delta = delta
      )

      sol_log <- tryCatch(
        run_stan_harness(model_logit_harness, data = dat),
        error = function(e) {
          message(sprintf(
            "Logit harness error on case %d: %s",
            case_idx,
            conditionMessage(e)
          ))
          NULL
        }
      )
      sol_prob <- tryCatch(
        run_stan_harness(model_probit_harness, data = dat),
        error = function(e) {
          message(sprintf(
            "Probit harness error on case %d: %s",
            case_idx,
            conditionMessage(e)
          ))
          NULL
        }
      )

      if (is.null(sol_log) || is.null(sol_prob)) {
        next
      }

      record_method <- function(method_name, link, est_vec, ref_vec) {
        est <- as.numeric(est_vec)
        param_err <- max(abs(est - ref_vec))
        rel_err <- max(abs(est - ref_vec) / (1 + abs(ref_vec)))
        res_err <- max(calc_residual(p0_vec, w, delta, est, link))

        data.table(
          case_id = case_idx,
          p0_regime = p_reg,
          sigma = sig,
          K = K,
          m2 = m2,
          m3 = m3,
          m4 = m4,
          max_delta = max(abs(delta)),
          link = link,
          method = method_name,
          param_error = param_err,
          rel_error = rel_err,
          residual_error = res_err
        )
      }

      results[[length(results) + 1L]] <- rbind(
        # Logit methods
        record_method("Taylor2", "logit", sol_log$logit_taylor2, ref_logit),
        record_method("Taylor4", "logit", sol_log$logit_taylor4, ref_logit),
        record_method("Asymptotic", "logit", sol_log$logit_asymp, ref_logit),
        record_method("MGF", "logit", sol_log$logit_mgf, ref_logit),
        record_method(
          "Conditioned_Guess",
          "logit",
          sol_log$logit_cond,
          ref_logit
        ),
        record_method(
          "Newton_Warm_T4_1step",
          "logit",
          sol_log$logit_newton_warm1,
          ref_logit
        ),
        record_method(
          "Newton_Warm_T4_2step",
          "logit",
          sol_log$logit_newton_warm2,
          ref_logit
        ),
        record_method(
          "Newton_Warm_Cond_1step",
          "logit",
          sol_log$logit_newton_cond1,
          ref_logit
        ),
        record_method(
          "Newton_Warm_Cond_2step",
          "logit",
          sol_log$logit_newton_cond2,
          ref_logit
        ),
        record_method(
          "Newton_Warm_MGF_1step",
          "logit",
          sol_log$logit_newton_mgf1,
          ref_logit
        ),
        record_method(
          "Newton_Warm_MGF_2step",
          "logit",
          sol_log$logit_newton_mgf2,
          ref_logit
        ),
        record_method(
          "Newton_Naive_10step",
          "logit",
          sol_log$logit_newton_naive,
          ref_logit
        ),
        record_method(
          "Halley_Warm_T4_1step",
          "logit",
          sol_log$logit_halley_warm1,
          ref_logit
        ),
        record_method(
          "Halley_Warm_T4_2step",
          "logit",
          sol_log$logit_halley_warm2,
          ref_logit
        ),
        record_method(
          "Halley_Warm_Cond_1step",
          "logit",
          sol_log$logit_halley_cond1,
          ref_logit
        ),
        record_method(
          "Halley_Warm_Cond_2step",
          "logit",
          sol_log$logit_halley_cond2,
          ref_logit
        ),
        record_method(
          "Halley_Warm_MGF_1step",
          "logit",
          sol_log$logit_halley_mgf1,
          ref_logit
        ),
        record_method(
          "Halley_Warm_MGF_2step",
          "logit",
          sol_log$logit_halley_mgf2,
          ref_logit
        ),
        record_method(
          "Halley_Warm_MGF_max10",
          "logit",
          sol_log$logit_halley_mgf_max10,
          ref_logit
        ),
        record_method(
          "Halley_Warm_Cond_max10",
          "logit",
          sol_log$logit_halley_cond_max10,
          ref_logit
        ),
        record_method(
          "Halley_Naive_10step",
          "logit",
          sol_log$logit_halley_naive,
          ref_logit
        ),
        record_method(
          "Stan_Builtin_Naive",
          "logit",
          sol_log$logit_builtin_naive,
          ref_logit
        ),
        record_method(
          "Stan_Builtin_Warm_T4",
          "logit",
          sol_log$logit_builtin_warm,
          ref_logit
        ),
        record_method(
          "Stan_Builtin_Warm_Cond",
          "logit",
          sol_log$logit_builtin_cond,
          ref_logit
        ),
        record_method(
          "Stan_Builtin_Warm_MGF",
          "logit",
          sol_log$logit_builtin_mgf,
          ref_logit
        ),
        # Probit methods
        record_method("Taylor2", "probit", sol_prob$probit_taylor2, ref_probit),
        record_method("Taylor4", "probit", sol_prob$probit_taylor4, ref_probit),
        record_method(
          "Asymptotic",
          "probit",
          sol_prob$probit_asymp,
          ref_probit
        ),
        record_method("Pade11", "probit", sol_prob$probit_pade11, ref_probit),
        record_method(
          "Conditioned_Guess",
          "probit",
          sol_prob$probit_cond,
          ref_probit
        ),
        record_method(
          "Newton_Warm_T4_1step",
          "probit",
          sol_prob$probit_newton_warm1,
          ref_probit
        ),
        record_method(
          "Newton_Warm_T4_2step",
          "probit",
          sol_prob$probit_newton_warm2,
          ref_probit
        ),
        record_method(
          "Newton_Warm_Cond_1step",
          "probit",
          sol_prob$probit_newton_cond1,
          ref_probit
        ),
        record_method(
          "Newton_Warm_Cond_2step",
          "probit",
          sol_prob$probit_newton_cond2,
          ref_probit
        ),
        record_method(
          "Newton_Warm_Pade_1step",
          "probit",
          sol_prob$probit_newton_pade1,
          ref_probit
        ),
        record_method(
          "Newton_Warm_Pade_2step",
          "probit",
          sol_prob$probit_newton_pade2,
          ref_probit
        ),
        record_method(
          "Newton_Naive_10step",
          "probit",
          sol_prob$probit_newton_naive,
          ref_probit
        ),
        record_method(
          "Halley_Warm_T4_1step",
          "probit",
          sol_prob$probit_halley_warm1,
          ref_probit
        ),
        record_method(
          "Halley_Warm_T4_2step",
          "probit",
          sol_prob$probit_halley_warm2,
          ref_probit
        ),
        record_method(
          "Halley_Warm_Cond_1step",
          "probit",
          sol_prob$probit_halley_cond1,
          ref_probit
        ),
        record_method(
          "Halley_Warm_Cond_2step",
          "probit",
          sol_prob$probit_halley_cond2,
          ref_probit
        ),
        record_method(
          "Halley_Warm_Pade_1step",
          "probit",
          sol_prob$probit_halley_pade1,
          ref_probit
        ),
        record_method(
          "Halley_Warm_Pade_2step",
          "probit",
          sol_prob$probit_halley_pade2,
          ref_probit
        ),
        record_method(
          "Halley_Warm_Pade_max10",
          "probit",
          sol_prob$probit_halley_pade_max10,
          ref_probit
        ),
        record_method(
          "Halley_Warm_Cond_max10",
          "probit",
          sol_prob$probit_halley_cond_max10,
          ref_probit
        ),
        record_method(
          "Halley_Naive_10step",
          "probit",
          sol_prob$halley_naive,
          ref_probit
        ),
        record_method(
          "Stan_Builtin_Naive",
          "probit",
          sol_prob$probit_builtin_naive,
          ref_probit
        ),
        record_method(
          "Stan_Builtin_Warm_T4",
          "probit",
          sol_prob$probit_builtin_warm,
          ref_probit
        ),
        record_method(
          "Stan_Builtin_Warm_Cond",
          "probit",
          sol_prob$probit_builtin_cond,
          ref_probit
        ),
        record_method(
          "Stan_Builtin_Warm_Pade",
          "probit",
          sol_prob$probit_builtin_pade,
          ref_probit
        )
      )
    }
  }
}

full_benchmark_dt <- rbindlist(results)
saveRDS(full_benchmark_dt, "data-raw/benchmark_solver_comprehensive.rds")
cat("Completed evaluation! Total rows:", nrow(full_benchmark_dt), "\n")
