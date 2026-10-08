#' Comprehensive Benchmark of Inference Solvers and Initial Guesses in imuGAP
#'
#' Evaluates:
#'  1. Simulated 2-layer hierarchical populations across:
#'     - Link functions: Logit and Probit (matched inference to generation)
#'     - Enclosing top-level probabilities: p0 in [0.5, 1.0]
#'     - Subpopulation sizes: K in {3, 10, 30, 100}
#'     - Offset dispersion: sigma in {0.2, 0.6, 1.0, 1.4}
#'     - Low-noise observations (most likely observation counts)
#'     - n = 100 sample populations per (link, K, sigma) configuration
#'     - Solvers and guesses: Zero, Taylor 2, Taylor 4, Pade/MGF, Conditioned,
#'       Halley (2 & 10 steps), Newton (2 steps), Stan Built-in (algebra_solver)
#'  2. Inference gradient/leapfrog evaluation speed and MCMC runtime.
#'  3. Stability and timing verification on real package sample data.

suppressPackageStartupMessages({
  library(data.table)
  library(microbenchmark)
  library(rstan)
  library(imuGAP)
  pkgload::load_all(quiet = TRUE)
})

source("tests/testthat/helper-stan-test.R")

cat("=== Initializing Comprehensive Inference Benchmark ===\n")

# --- Step 1: Compile Solver Evaluation Harnesses -----------------------------
cat("Compiling Stan solver harnesses...\n")

model_bench_logit <- sprintf(
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
  vector[C] p0_c = clamp_probability(p0);
  vector[C] eta0 = link_fn(p0_c);

  // Initial Guesses
  vector[C] z_guess = shift_zero(eta0, p0_c, w, delta);
  vector[C] t2_guess = shift_logit_taylor2(eta0, p0_c, w, delta);
  vector[C] t4_guess = shift_logit_taylor4(eta0, p0_c, w, delta);
  vector[C] mgf_guess = shift_logit_mgf(eta0, p0_c, w, delta);
  vector[C] cond_guess = shift_logit_conditioned(eta0, p0_c, w, delta);

  // Iterative Solvers
  vector[C] halley_w2 = solve_shift_halley(cond_guess, eta0, p0_c, w, delta, 2);
  vector[C] halley_w10 = solve_shift_halley(cond_guess, eta0, p0_c, w, delta, 10);
  vector[C] halley_naive10 = solve_shift_halley(z_guess, eta0, p0_c, w, delta, 10);
  vector[C] newton_w2 = solve_shift_newton(cond_guess, eta0, p0_c, w, delta, 2);
  vector[C] builtin_w = solve_shift_builtin(cond_guess, eta0, p0_c, w, delta);
  vector[C] builtin_naive = solve_shift_builtin(z_guess, eta0, p0_c, w, delta);
}
"
) |>
  compile_stan_harness()

model_bench_probit <- sprintf(
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
  vector[C] p0_c = clamp_probability(p0);
  vector[C] eta0 = link_fn(p0_c);

  // Initial Guesses
  vector[C] z_guess = shift_zero(eta0, p0_c, w, delta);
  vector[C] t2_guess = shift_probit_taylor2(eta0, p0_c, w, delta);
  vector[C] t4_guess = shift_probit_taylor4(eta0, p0_c, w, delta);
  vector[C] pade_guess = shift_probit_pade11(eta0, p0_c, w, delta);
  vector[C] cond_guess = shift_probit_conditioned(eta0, p0_c, w, delta);

  // Iterative Solvers
  vector[C] halley_w2 = solve_shift_halley(cond_guess, eta0, p0_c, w, delta, 2);
  vector[C] halley_w10 = solve_shift_halley(cond_guess, eta0, p0_c, w, delta, 10);
  vector[C] halley_naive10 = solve_shift_halley(z_guess, eta0, p0_c, w, delta, 10);
  vector[C] newton_w2 = solve_shift_newton(cond_guess, eta0, p0_c, w, delta, 2);
  vector[C] builtin_w = solve_shift_builtin(cond_guess, eta0, p0_c, w, delta);
  vector[C] builtin_naive = solve_shift_builtin(z_guess, eta0, p0_c, w, delta);
}
"
) |>
  compile_stan_harness()

# Reference high-precision numerical solver in R
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
        interval = c(-15, 15),
        tol = 1e-14
      )$root
    },
    numeric(1)
  )
}

calc_residual_err <- function(p0, w, delta, mu, link = c("logit", "probit")) {
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

# --- Step 2: Factorial Simulation of 2-Layer Populations ---------------------
cat(
  "Generating synthetic 2-layer populations and evaluating solver errors...\n"
)

links <- c("logit", "probit")
k_vals <- c(3L, 10L, 30L, 100L)
sigma_vals <- c(0.2, 0.6, 1.0, 1.4)
n_pops <- 100L

set.seed(20261007)
sim_records <- list()

for (lnk in links) {
  harness_model <- if (lnk == "logit") model_bench_logit else model_bench_probit

  for (K in k_vals) {
    for (sig in sigma_vals) {
      for (pop_idx in seq_len(n_pops)) {
        # Top level probability in [0.5, 1.0]
        p0_val <- stats::runif(1L, 0.5, 0.999)
        p0_vec <- c(p0_val)
        C <- 1L

        # Random population weights
        w_raw <- stats::runif(K, 0.5, 2.0)
        w <- w_raw / sum(w_raw)

        # Offsets
        delta_raw <- stats::rnorm(K, mean = 0, sd = sig)
        delta <- delta_raw - sum(w * delta_raw)

        # Reference truth
        mu_true <- solve_exact_r(p0_vec, w, delta, link = lnk)

        # Low-noise observations (most likely observations)
        n_subpop <- round(stats::runif(K, 1000, 5000))
        inv_fn <- if (lnk == "logit") stats::plogis else stats::pnorm
        link_fn <- if (lnk == "logit") stats::qlogis else stats::qnorm
        p_subpop <- inv_fn(link_fn(p0_val) + mu_true + delta)
        y_obs <- round(n_subpop * p_subpop)

        dat <- list(
          C = C,
          K = K,
          p0 = array(p0_vec, dim = C),
          w = array(w, dim = K),
          delta = array(delta, dim = K)
        )

        res <- run_stan_harness(harness_model, data = dat)

        # Record metrics for each method
        record_res <- function(method_name, est_mu) {
          est <- as.numeric(est_mu)
          param_err <- abs(est - mu_true)
          res_err <- calc_residual_err(p0_vec, w, delta, est, lnk)

          data.table(
            link = lnk,
            K = K,
            sigma = sig,
            pop_id = pop_idx,
            p0 = p0_val,
            method = method_name,
            param_error = param_err,
            residual_error = max(res_err, 1e-16)
          )
        }

        sim_records[[length(sim_records) + 1L]] <- rbind(
          record_res("Zero Shift", res$z_guess),
          record_res("Taylor 2nd", res$t2_guess),
          record_res("Taylor 4th", res$t4_guess),
          record_res(
            "Padé / MGF",
            if (lnk == "logit") res$mgf_guess else res$pade_guess
          ),
          record_res("Conditioned", res$cond_guess),
          record_res("Halley (2 Steps)", res$halley_w2),
          record_res("Halley (10 Steps)", res$halley_w10),
          record_res("Halley (Naive 10)", res$halley_naive10),
          record_res("Newton (2 Steps)", res$newton_w2),
          record_res("Stan Builtin (Warm)", res$builtin_w),
          record_res("Stan Builtin (Naive)", res$builtin_naive)
        )
      }
    }
  }
}

sim_dt <- rbindlist(sim_records)
cat(sprintf("Generated and evaluated %d simulation records.\n", nrow(sim_dt)))

# --- Step 3: Compile Full Inference Stan Models with Parameterized Solvers ----
cat(
  "Compiling full hierarchical Stan inference models with distinct solvers...\n"
)

compile_full_inference_model <- function(
  link = c("logit", "probit"),
  guess = c("zero", "taylor2", "taylor4", "conditioned", "asymptotic"),
  solver = c("direct", "halley2", "halley10", "newton2", "builtin")
) {
  link <- match.arg(link)
  guess <- match.arg(guess)
  solver <- match.arg(solver)
  tmpl_path <- file.path(
    rprojroot::find_package_root_file(),
    "inst",
    "stan",
    "templates",
    "bspline_static_offsets.stan.template"
  )
  tmpl <- paste(readLines(tmpl_path, warn = FALSE), collapse = "\n")

  stan_code <- tmpl |>
    gsub(pattern = "@LINK@", replacement = link, fixed = TRUE) |>
    gsub(pattern = "@GUESS@", replacement = guess, fixed = TRUE) |>
    gsub(pattern = "@SOLVE@", replacement = solver, fixed = TRUE)

  compile_stan_harness(stan_code)
}

# Compile representative solver models for inference benchmarking
models_inf <- list()

cat("Compiling Zero Shift Logit inference model...\n")
models_inf[["logit_zero"]] <- compile_full_inference_model(
  "logit",
  "zero",
  "direct"
)

cat("Compiling Taylor 2nd Logit inference model...\n")
models_inf[["logit_taylor2"]] <- compile_full_inference_model(
  "logit",
  "taylor2",
  "direct"
)

cat("Compiling Taylor 4th Logit inference model...\n")
models_inf[["logit_taylor4"]] <- compile_full_inference_model(
  "logit",
  "taylor4",
  "direct"
)

cat("Compiling Conditioned Logit inference model...\n")
models_inf[["logit_conditioned"]] <- compile_full_inference_model(
  "logit",
  "conditioned",
  "direct"
)

cat("Compiling Halley (2 Steps) Logit inference model...\n")
models_inf[["logit_halley2"]] <- compile_full_inference_model(
  "logit",
  "conditioned",
  "halley2"
)

cat("Compiling Halley (10 Steps) Logit inference model...\n")
models_inf[["logit_halley10"]] <- compile_full_inference_model(
  "logit",
  "zero",
  "halley10"
)

cat("Compiling Newton (2 Steps) Logit inference model...\n")
models_inf[["logit_newton2"]] <- compile_full_inference_model(
  "logit",
  "taylor2",
  "newton2"
)

cat("Compiling Stan Built-in Logit inference model...\n")
models_inf[["logit_builtin"]] <- compile_full_inference_model(
  "logit",
  "conditioned",
  "builtin"
)

# Probit representative models
cat("Compiling Taylor 4th Probit inference model...\n")
models_inf[["probit_taylor4"]] <- compile_full_inference_model(
  "probit",
  "taylor4",
  "direct"
)

cat("Compiling Halley (2 Steps) Probit inference model...\n")
models_inf[["probit_halley2"]] <- compile_full_inference_model(
  "probit",
  "conditioned",
  "halley2"
)

# --- Step 4: Real Data Inference Benchmarking & Stability ---------------------
cat(
  "\nRunning inference benchmark across solvers on real package sample data...\n"
)

data("locations_sim", package = "imuGAP")
data("populations_sim", package = "imuGAP")
data("observations_sim", package = "imuGAP")

# Prepare 2-layer slice
locations_sim_2layer <- locations_sim[is.na(parent_id) | parent_id == "State"]
populations_sim_2layer <- copy(populations_sim)
loc_map_2layer <- locations_sim[!is.na(parent_id), .(loc_id, parent_id)]
populations_sim_2layer[loc_map_2layer, on = .(loc_id), loc_id := i.parent_id]
populations_sim_2layer <- populations_sim_2layer[,
  .(weight = sum(weight)),
  by = .(obs_id, loc_id, cohort, age, dose)
]
observations_sim_2layer <- copy(observations_sim)

# Prepare Stan data using package canonicalizers
loc_info_2layer <- canonicalize_locations(locations_sim_2layer)
obs_2layer <- canonicalize_observations(observations_sim_2layer)
wts_2layer <- canonicalize_populations(
  populations_sim_2layer,
  obs_2layer,
  loc_info_2layer,
  imugap_opts = imugap_options()
)
bsp <- splines::bs(
  seq_len(wts_2layer[, diff(range(cohort)) + 1L]),
  df = 5L,
  intercept = TRUE
)
sched_info <- build_interval_schedule(c(1L, 4L), wts_2layer$age)
st_uncensored <- slice_weights(
  wts_2layer,
  obs_2layer[is.na(censored)],
  "uncensored"
)
st_right <- slice_weights(wts_2layer, obs_2layer[censored == 1], "right")
st_left <- slice_weights(wts_2layer, obs_2layer[0], "left")
layer_data <- assemble_layer_data(loc_info_2layer)

dat_stan_2layer <- c(
  list(
    n_yr = max(wts_2layer$age),
    n_cohort = max(wts_2layer$cohort)
  ),
  layer_data,
  list(
    n_doses = 2L,
    n_intervals = sched_info$n_intervals,
    dt_vec = sched_info$dt_vec,
    dose_sched = sched_info$dose_sched,
    age_to_interval_map = sched_info$age_to_interval_map,
    k_bs = ncol(bsp),
    bs = bsp
  ),
  st_uncensored,
  st_right,
  st_left,
  list(
    predict_mode = 0L,
    compute_log_lik = 0L,
    num_threads = 1L
  )
)

real_inference_records <- list()
init_fit <- NULL

# Deterministic test MCMC configuration
chains <- 2L
iter <- 300L
warmup <- 150L
seed <- 42L

for (m_key in names(models_inf)) {
  sm <- models_inf[[m_key]]
  cat(sprintf("Sampling with solver model: %s ...\n", m_key))

  t0 <- proc.time()
  fit_mcmc <- suppressWarnings(rstan::sampling(
    sm,
    data = dat_stan_2layer,
    chains = chains,
    iter = iter,
    warmup = warmup,
    seed = seed,
    refresh = 0L
  ))
  t1 <- proc.time()
  wall_time <- (t1 - t0)["elapsed"]

  sum_mat <- rstan::summary(fit_mcmc)$summary
  rhat_col <- if ("Rhat" %in% colnames(sum_mat)) "Rhat" else "rhat"
  ess_col <- if ("n_eff" %in% colnames(sum_mat)) "n_eff" else "ess_bulk"

  max_rhat <- max(sum_mat[, rhat_col], na.rm = TRUE)
  min_ess <- min(sum_mat[, ess_col], na.rm = TRUE)
  med_ess <- stats::median(sum_mat[, ess_col], na.rm = TRUE)
  divs <- rstan::get_num_divergent(fit_mcmc)

  chain_times <- rstan::get_elapsed_time(fit_mcmc)
  warmup_mean <- mean(chain_times[, "warmup"])
  sampling_mean <- mean(chain_times[, "sample"])
  total_chain_mean <- mean(rowSums(chain_times))

  # Extract posterior means of beta_bs and sigma_layer
  ext <- rstan::extract(fit_mcmc)
  beta_mean <- colMeans(ext$beta_bs)
  sigma_mean <- mean(ext$sigma_layer)

  real_inference_records[[length(real_inference_records) + 1L]] <- data.table(
    model_key = m_key,
    wall_clock_s = wall_time,
    warmup_s = warmup_mean,
    sampling_s = sampling_mean,
    total_chain_s = total_chain_mean,
    max_rhat = max_rhat,
    min_ess = min_ess,
    median_ess = med_ess,
    divergences = divs,
    sigma_mean = sigma_mean,
    beta_1 = beta_mean[1],
    beta_2 = beta_mean[2],
    beta_3 = beta_mean[3],
    beta_4 = beta_mean[4],
    beta_5 = beta_mean[5]
  )
}

real_inf_dt <- rbindlist(real_inference_records)
cat("\nReal Data Fit Inference Benchmark Summary:\n")
print(real_inf_dt[, .(
  model_key,
  wall_clock_s = round(wall_clock_s, 2),
  sampling_s = round(sampling_s, 2),
  max_rhat = round(max_rhat, 3),
  min_ess = round(min_ess, 1),
  divergences
)])

# --- Step 5: Save Benchmark Results ------------------------------------------
benchmark_results <- list(
  sim_errors = sim_dt,
  real_inference = real_inf_dt,
  meta = list(
    date = Sys.Date(),
    links = links,
    k_vals = k_vals,
    sigma_vals = sigma_vals,
    n_pops = n_pops
  )
)

saveRDS(benchmark_results, "data-raw/benchmark_inference_results.rds")
cat("\nBenchmark artifacts saved to data-raw/benchmark_inference_results.rds\n")
