#' Focused Benchmark Comparison: Warm-Conditioned vs Naive, and Stan Built-in vs Halley
#' Compares:
#'  1. Halley Warm-Conditioned (max 2 steps, tol=1e-12)
#'  2. Halley Warm-Conditioned (max 10 steps, tol=1e-12)
#'  3. Halley Naive (max 2 steps, tol=1e-12)
#'  4. Halley Naive (max 10 steps, tol=1e-12)
#'  5. Stan Built-in (Warm-Conditioned)
#'  6. Stan Built-in (Naive 0-start)
#' Across Logit and Probit links.

suppressPackageStartupMessages({
  library(data.table)
  library(microbenchmark)
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
  "functions/solvers/haste_halley.stan",
  "functions/solvers/builtin_rootfinding.stan"
)

cat("Compiling focused Stan harnesses...\n")
model_focused_logit <- sprintf(
  "
functions {
  #include functions/clamp_probability.stan
  #include functions/link/logit.stan
  #include functions/guess/taylor.stan
  #include functions/guess/pade_mgf.stan
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
parameters { real dummy; }
model { dummy ~ normal(0, 1); }
generated quantities {
  vector[C] p0_c = clamp_probability(p0);
  vector[C] eta0 = link_fn(p0_c);
  vector[C] z_guess = shift_zero(eta0, p0_c, w, delta);
  vector[C] cond_guess = shift_logit_conditioned(eta0, p0_c, w, delta);

  vector[C] halley_warm_max2 = solve_shift_halley(cond_guess, eta0, p0_c, w, delta, 2, 1e-12);
  vector[C] halley_warm_max10 = solve_shift_halley(cond_guess, eta0, p0_c, w, delta, 10, 1e-12);
  vector[C] halley_naive_max2 = solve_shift_halley(z_guess, eta0, p0_c, w, delta, 2, 1e-12);
  vector[C] halley_naive_max10 = solve_shift_halley(z_guess, eta0, p0_c, w, delta, 10, 1e-12);
  vector[C] builtin_warm = solve_shift_builtin(cond_guess, eta0, p0_c, w, delta);
  vector[C] builtin_naive = solve_shift_builtin(z_guess, eta0, p0_c, w, delta);
}
"
) |>
  compile_stan_harness()

model_focused_probit <- sprintf(
  "
functions {
  #include functions/clamp_probability.stan
  #include functions/link/probit.stan
  #include functions/guess/taylor.stan
  #include functions/guess/pade_mgf.stan
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
parameters { real dummy; }
model { dummy ~ normal(0, 1); }
generated quantities {
  vector[C] p0_c = clamp_probability(p0);
  vector[C] eta0 = link_fn(p0_c);
  vector[C] z_guess = shift_zero(eta0, p0_c, w, delta);
  vector[C] cond_guess = shift_probit_conditioned(eta0, p0_c, w, delta);

  vector[C] halley_warm_max2 = solve_shift_halley(cond_guess, eta0, p0_c, w, delta, 2, 1e-12);
  vector[C] halley_warm_max10 = solve_shift_halley(cond_guess, eta0, p0_c, w, delta, 10, 1e-12);
  vector[C] halley_naive_max2 = solve_shift_halley(z_guess, eta0, p0_c, w, delta, 2, 1e-12);
  vector[C] halley_naive_max10 = solve_shift_halley(z_guess, eta0, p0_c, w, delta, 10, 1e-12);
  vector[C] builtin_warm = solve_shift_builtin(cond_guess, eta0, p0_c, w, delta);
  vector[C] builtin_naive = solve_shift_builtin(z_guess, eta0, p0_c, w, delta);
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
  edge = c(0.05, 0.95),
  extreme = c(0.005, 0.01, 0.99, 0.995)
)

sigma_vals <- c(0.1, 0.3, 0.6, 1.0, 1.5, 2.0)
K_vals <- c(2L, 5L, 10L, 25L, 50L)

results <- list()
set.seed(42)

cat("Running focused factorial evaluation...\n")
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
        run_stan_harness(model_focused_logit, data = dat),
        error = function(e) {
          message(sprintf(
            "Logit error on case %d: %s",
            case_idx,
            conditionMessage(e)
          ))
          NULL
        }
      )
      sol_prob <- tryCatch(
        run_stan_harness(model_focused_probit, data = dat),
        error = function(e) {
          message(sprintf(
            "Probit error on case %d: %s",
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
        res_err <- max(calc_residual(p0_vec, w, delta, est, link))

        data.table(
          case_id = case_idx,
          p0_regime = p_reg,
          sigma = sig,
          K = K,
          link = link,
          method = method_name,
          param_error = param_err,
          residual_error = res_err
        )
      }

      results[[length(results) + 1L]] <- rbind(
        # Logit
        record_method(
          "Halley_Warm_Cond_max2",
          "logit",
          sol_log$halley_warm_max2,
          ref_logit
        ),
        record_method(
          "Halley_Warm_Cond_max10",
          "logit",
          sol_log$halley_warm_max10,
          ref_logit
        ),
        record_method(
          "Halley_Naive_max2",
          "logit",
          sol_log$halley_naive_max2,
          ref_logit
        ),
        record_method(
          "Halley_Naive_max10",
          "logit",
          sol_log$halley_naive_max10,
          ref_logit
        ),
        record_method(
          "Stan_Builtin_Warm_Cond",
          "logit",
          sol_log$builtin_warm,
          ref_logit
        ),
        record_method(
          "Stan_Builtin_Naive",
          "logit",
          sol_log$builtin_naive,
          ref_logit
        ),
        # Probit
        record_method(
          "Halley_Warm_Cond_max2",
          "probit",
          sol_prob$halley_warm_max2,
          ref_probit
        ),
        record_method(
          "Halley_Warm_Cond_max10",
          "probit",
          sol_prob$halley_warm_max10,
          ref_probit
        ),
        record_method(
          "Halley_Naive_max2",
          "probit",
          sol_prob$halley_naive_max2,
          ref_probit
        ),
        record_method(
          "Halley_Naive_max10",
          "probit",
          sol_prob$halley_naive_max10,
          ref_probit
        ),
        record_method(
          "Stan_Builtin_Warm_Cond",
          "probit",
          sol_prob$builtin_warm,
          ref_probit
        ),
        record_method(
          "Stan_Builtin_Naive",
          "probit",
          sol_prob$builtin_naive,
          ref_probit
        )
      )
    }
  }
}

focused_dt <- rbindlist(results)
saveRDS(focused_dt, "data-raw/benchmark_focused_results.rds")

# Summary across full grid
summary_focused <- focused_dt[,
  .(
    median_residual = median(residual_error),
    max_residual = max(residual_error),
    median_param_err = median(param_error),
    max_param_err = max(param_error),
    p99_residual = quantile(residual_error, 0.99)
  ),
  by = .(link, method)
]

print(summary_focused[order(link, median_residual)])
