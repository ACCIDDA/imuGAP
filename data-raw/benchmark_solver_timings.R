#' Execution Speed Microbenchmark for Link Solvers in Stan
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

# Test input vector setup
C <- 5L
K <- 10L
p0_vec <- c(0.1, 0.25, 0.5, 0.75, 0.9)
w_raw <- runif(K, 0.5, 2.0)
w <- w_raw / sum(w_raw)
delta_raw <- rnorm(K, 0, 0.5)
delta <- delta_raw - sum(w * delta_raw)

dat <- list(
  C = C,
  K = K,
  p0 = p0_vec,
  w = w,
  delta = delta
)

# Single-evaluation Stan models with modular includes and unified entry point
compile_solver <- function(includes, body_expr) {
  sprintf(
    "
functions {
  %s

  vector solve_shift(vector p0, vector w, vector delta) {
    vector[num_elements(p0)] p0_c = clamp_probability(p0);
    vector[num_elements(p0)] eta0 = link_fn(p0_c);
    return %s;
  }
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
  vector[C] out = solve_shift(p0, w, delta);
}
",
    paste(sprintf("#include %s", unique(includes)), collapse = "\n  "),
    body_expr
  ) |>
    compile_stan_harness()
}

inc_clamp <- "functions/clamp_probability.stan"
inc_link_logit <- "functions/link/logit.stan"
inc_link_probit <- "functions/link/probit.stan"

inc_taylor_logit <- c(inc_clamp, inc_link_logit, "functions/guess/taylor.stan")
inc_cond_logit <- c(
  inc_clamp,
  inc_link_logit,
  "functions/guess/taylor.stan",
  "functions/guess/pade_mgf.stan",
  "functions/guess/conditioned_guess.stan"
)

inc_taylor_probit <- c(
  inc_clamp,
  inc_link_probit,
  "functions/guess/taylor.stan"
)
inc_cond_probit <- c(
  inc_clamp,
  inc_link_probit,
  "functions/guess/taylor.stan",
  "functions/guess/pade_mgf.stan",
  "functions/guess/conditioned_guess.stan"
)

# Solver includes
inc_newton_base <- "functions/solvers/newton.stan"
inc_halley_base <- "functions/solvers/haste_halley.stan"
inc_builtin_base <- "functions/solvers/builtin_rootfinding.stan"

# Logit include bundles
inc_log_newton_pure <- c(inc_clamp, inc_link_logit, inc_newton_base)
inc_log_newton_t4 <- c(inc_log_newton_pure, inc_taylor_logit)
inc_log_newton_cond <- c(inc_log_newton_pure, inc_cond_logit)

inc_log_halley_pure <- c(inc_clamp, inc_link_logit, inc_halley_base)
inc_log_halley_t4 <- c(inc_log_halley_pure, inc_taylor_logit)
inc_log_halley_cond <- c(inc_log_halley_pure, inc_cond_logit)

inc_log_builtin_pure <- c(inc_clamp, inc_link_logit, inc_builtin_base)
inc_log_builtin_t4 <- c(inc_log_builtin_pure, inc_taylor_logit)
inc_log_builtin_cond <- c(inc_log_builtin_pure, inc_cond_logit)

# Probit include bundles
inc_prob_newton_pure <- c(inc_clamp, inc_link_probit, inc_newton_base)
inc_prob_newton_t4 <- c(inc_prob_newton_pure, inc_taylor_probit)
inc_prob_newton_cond <- c(inc_prob_newton_pure, inc_cond_probit)

inc_prob_halley_pure <- c(inc_clamp, inc_link_probit, inc_halley_base)
inc_prob_halley_t4 <- c(inc_prob_halley_pure, inc_taylor_probit)
inc_prob_halley_cond <- c(inc_prob_halley_pure, inc_cond_probit)

inc_prob_builtin_pure <- c(inc_clamp, inc_link_probit, inc_builtin_base)
inc_prob_builtin_t4 <- c(inc_prob_builtin_pure, inc_taylor_probit)
inc_prob_builtin_cond <- c(inc_prob_builtin_pure, inc_cond_probit)

cat("Compiling isolated Stan models for timing benchmarks...\n")

# Logit models
m_log_t2 <- compile_solver(
  inc_taylor_logit,
  "shift_logit_taylor2(eta0, p0_c, w, delta)"
)
m_log_t4 <- compile_solver(
  inc_taylor_logit,
  "shift_logit_taylor4(eta0, p0_c, w, delta)"
)
m_log_cond <- compile_solver(
  inc_cond_logit,
  "shift_logit_conditioned(eta0, p0_c, w, delta)"
)

m_log_newton_w1 <- compile_solver(
  inc_log_newton_t4,
  "solve_shift_newton(shift_logit_taylor4(eta0, p0_c, w, delta), eta0, p0_c, w, delta, 1, 1e-12)"
)
m_log_newton_w2 <- compile_solver(
  inc_log_newton_t4,
  "solve_shift_newton(shift_logit_taylor4(eta0, p0_c, w, delta), eta0, p0_c, w, delta, 2, 1e-12)"
)
m_log_newton_c2 <- compile_solver(
  inc_log_newton_cond,
  "solve_shift_newton(shift_logit_conditioned(eta0, p0_c, w, delta), eta0, p0_c, w, delta, 2, 1e-12)"
)
m_log_newton_naive <- compile_solver(
  inc_log_newton_pure,
  "solve_shift_newton(rep_vector(0.0, num_elements(p0)), eta0, p0_c, w, delta, 10, 1e-12)"
)

m_log_halley_w1 <- compile_solver(
  inc_log_halley_t4,
  "solve_shift_halley(shift_logit_taylor4(eta0, p0_c, w, delta), eta0, p0_c, w, delta, 1, 1e-12)"
)
m_log_halley_w2 <- compile_solver(
  inc_log_halley_t4,
  "solve_shift_halley(shift_logit_taylor4(eta0, p0_c, w, delta), eta0, p0_c, w, delta, 2, 1e-12)"
)
m_log_halley_c2 <- compile_solver(
  inc_log_halley_cond,
  "solve_shift_halley(shift_logit_conditioned(eta0, p0_c, w, delta), eta0, p0_c, w, delta, 2, 1e-12)"
)
m_log_halley_naive <- compile_solver(
  inc_log_halley_pure,
  "solve_shift_halley(rep_vector(0.0, num_elements(p0)), eta0, p0_c, w, delta, 10, 1e-12)"
)

m_log_builtin_naive <- compile_solver(
  inc_log_builtin_pure,
  "solve_shift_builtin(rep_vector(0.0, num_elements(p0)), eta0, p0_c, w, delta)"
)
m_log_builtin_warm <- compile_solver(
  inc_log_builtin_t4,
  "solve_shift_builtin(shift_logit_taylor4(eta0, p0_c, w, delta), eta0, p0_c, w, delta)"
)
m_log_builtin_cond <- compile_solver(
  inc_log_builtin_cond,
  "solve_shift_builtin(shift_logit_conditioned(eta0, p0_c, w, delta), eta0, p0_c, w, delta)"
)

# Probit models
m_prob_t2 <- compile_solver(
  inc_taylor_probit,
  "shift_probit_taylor2(eta0, p0_c, w, delta)"
)
m_prob_t4 <- compile_solver(
  inc_taylor_probit,
  "shift_probit_taylor4(eta0, p0_c, w, delta)"
)
m_prob_cond <- compile_solver(
  inc_cond_probit,
  "shift_probit_conditioned(eta0, p0_c, w, delta)"
)

m_prob_newton_w1 <- compile_solver(
  inc_prob_newton_t4,
  "solve_shift_newton(shift_probit_taylor4(eta0, p0_c, w, delta), eta0, p0_c, w, delta, 1, 1e-12)"
)
m_prob_newton_w2 <- compile_solver(
  inc_prob_newton_t4,
  "solve_shift_newton(shift_probit_taylor4(eta0, p0_c, w, delta), eta0, p0_c, w, delta, 2, 1e-12)"
)
m_prob_newton_c2 <- compile_solver(
  inc_prob_newton_cond,
  "solve_shift_newton(shift_probit_conditioned(eta0, p0_c, w, delta), eta0, p0_c, w, delta, 2, 1e-12)"
)
m_prob_newton_naive <- compile_solver(
  inc_prob_newton_pure,
  "solve_shift_newton(rep_vector(0.0, num_elements(p0)), eta0, p0_c, w, delta, 10, 1e-12)"
)

m_prob_halley_w1 <- compile_solver(
  inc_prob_halley_t4,
  "solve_shift_halley(shift_probit_taylor4(eta0, p0_c, w, delta), eta0, p0_c, w, delta, 1, 1e-12)"
)
m_prob_halley_w2 <- compile_solver(
  inc_prob_halley_t4,
  "solve_shift_halley(shift_probit_taylor4(eta0, p0_c, w, delta), eta0, p0_c, w, delta, 2, 1e-12)"
)
m_prob_halley_c2 <- compile_solver(
  inc_prob_halley_cond,
  "solve_shift_halley(shift_probit_conditioned(eta0, p0_c, w, delta), eta0, p0_c, w, delta, 2, 1e-12)"
)
m_prob_halley_naive <- compile_solver(
  inc_prob_halley_pure,
  "solve_shift_halley(rep_vector(0.0, num_elements(p0)), eta0, p0_c, w, delta, 10, 1e-12)"
)

m_prob_builtin_naive <- compile_solver(
  inc_prob_builtin_pure,
  "solve_shift_builtin(rep_vector(0.0, num_elements(p0)), eta0, p0_c, w, delta)"
)
m_prob_builtin_warm <- compile_solver(
  inc_prob_builtin_t4,
  "solve_shift_builtin(shift_probit_taylor4(eta0, p0_c, w, delta), eta0, p0_c, w, delta)"
)
m_prob_builtin_cond <- compile_solver(
  inc_prob_builtin_cond,
  "solve_shift_builtin(shift_probit_conditioned(eta0, p0_c, w, delta), eta0, p0_c, w, delta)"
)

cat("Running execution microbenchmark across solvers...\n")
bm <- microbenchmark(
  # Logit
  logit_Taylor2 = run_stan_harness(m_log_t2, dat),
  logit_Taylor4 = run_stan_harness(m_log_t4, dat),
  logit_Conditioned = run_stan_harness(m_log_cond, dat),
  logit_Newton_Warm_T4_1 = run_stan_harness(m_log_newton_w1, dat),
  logit_Newton_Warm_T4_2 = run_stan_harness(m_log_newton_w2, dat),
  logit_Newton_Warm_Cond_2 = run_stan_harness(m_log_newton_c2, dat),
  logit_Newton_Naive_10 = run_stan_harness(m_log_newton_naive, dat),
  logit_Halley_Warm_T4_1 = run_stan_harness(m_log_halley_w1, dat),
  logit_Halley_Warm_T4_2 = run_stan_harness(m_log_halley_w2, dat),
  logit_Halley_Warm_Cond_2 = run_stan_harness(m_log_halley_c2, dat),
  logit_Halley_Naive_10 = run_stan_harness(m_log_halley_naive, dat),
  logit_Builtin_Naive = run_stan_harness(m_log_builtin_naive, dat),
  logit_Builtin_Warm_T4 = run_stan_harness(m_log_builtin_warm, dat),
  logit_Builtin_Warm_Cond = run_stan_harness(m_log_builtin_cond, dat),
  # Probit
  probit_Taylor2 = run_stan_harness(m_prob_t2, dat),
  probit_Taylor4 = run_stan_harness(m_prob_t4, dat),
  probit_Conditioned = run_stan_harness(m_prob_cond, dat),
  probit_Newton_Warm_T4_1 = run_stan_harness(m_prob_newton_w1, dat),
  probit_Newton_Warm_T4_2 = run_stan_harness(m_prob_newton_w2, dat),
  probit_Newton_Warm_Cond_2 = run_stan_harness(m_prob_newton_c2, dat),
  probit_Newton_Naive_10 = run_stan_harness(m_prob_newton_naive, dat),
  probit_Halley_Warm_T4_1 = run_stan_harness(m_prob_halley_w1, dat),
  probit_Halley_Warm_T4_2 = run_stan_harness(m_prob_halley_w2, dat),
  probit_Halley_Warm_Cond_2 = run_stan_harness(m_prob_halley_c2, dat),
  probit_Halley_Naive_10 = run_stan_harness(m_prob_halley_naive, dat),
  probit_Builtin_Naive = run_stan_harness(m_prob_builtin_naive, dat),
  probit_Builtin_Warm_T4 = run_stan_harness(m_prob_builtin_warm, dat),
  probit_Builtin_Warm_Cond = run_stan_harness(m_prob_builtin_cond, dat),
  times = 25L
)

bm_summary <- as.data.table(summary(bm, unit = "ms"))
print(bm_summary)
saveRDS(bm_summary, "data-raw/benchmark_solver_timings.rds")
cat("Saved timings to data-raw/benchmark_solver_timings.rds\n")
