#' Benchmark Inference Runner for imuGAP Solvers & Initial Guesses
#'
#' Evaluates MCMC inference accuracy, coverage, and sampling efficiency across
#' synthetic population datasets and compiled Stan models specified in a configuration.
#'
#' Usage:
#'   Rscript benchmark_inference_runner.R <config.yml | config.rds> [output_results.rds] [options]
#'
#' Options:
#'   --chains=N     Number of MCMC chains (default: 2)
#'   --iter=N       Total iterations per chain (default: 300)
#'   --warmup=N     Warmup iterations per chain (default: 150)
#'   --cores=N      Parallel CPU cores for sampling (default: min(chains, available))
#'   --seed=N       Base PRNG seed (default: from config or 20261008)

suppressPackageStartupMessages({
  library(yaml)
  library(data.table)
  library(rstan)
  library(imuGAP)
  library(splines)
})

# Source compilation and Stan resolution helpers
if (file.exists("benchmark_compilation.R")) {
  source("benchmark_compilation.R")
} else if (file.exists("data-raw/benchmarks/benchmark_compilation.R")) {
  source("data-raw/benchmarks/benchmark_compilation.R")
} else if (file.exists("data-raw/benchmark_compilation.R")) {
  source("data-raw/benchmark_compilation.R")
}

# --- Metric Helper Functions --------------------------------------------------

#' Compute Bernoulli Jensen-Shannon Distance sqrt(JSD)
compute_bernoulli_jsd <- function(p_true, p_hat) {
  p_adj <- pmin(pmax(p_true, 1e-12), 1 - 1e-12)
  q_adj <- pmin(pmax(p_hat, 1e-12), 1 - 1e-12)
  m_adj <- 0.5 * (p_adj + q_adj)

  kl_p <- p_adj *
    log(p_adj / m_adj) +
    (1 - p_adj) * log((1 - p_adj) / (1 - m_adj))
  kl_q <- q_adj *
    log(q_adj / m_adj) +
    (1 - q_adj) * log((1 - q_adj) / (1 - m_adj))
  js_div <- 0.5 * (kl_p + kl_q)

  mean(sqrt(pmax(js_div, 0)))
}

#' Extract MCMC divergence count across chains
get_divergences <- function(fit) {
  sampler_params <- rstan::get_sampler_params(fit, inc_warmup = FALSE)
  sum(vapply(sampler_params, function(x) sum(x[, "divergent__"]), numeric(1)))
}

#' Prepare Stan input data list for 2-layer benchmark dataset
prepare_benchmark_stan_data <- function(ds, guess_type = 3L, solver_type = 1L) {
  locs <- ds$locations
  obs <- ds$observations
  pops <- ds$populations
  C <- if (!is.null(ds$p0_true)) length(ds$p0_true) else max(pops$cohort)

  # Canonicalize with imuGAP helpers
  loc_c <- canonicalize_locations(locs)
  layer_data <- imuGAP:::assemble_layer_data(
    loc_c,
    guess_type = guess_type,
    solver_type = solver_type
  )
  obs_c <- canonicalize_observations(obs)
  wts <- canonicalize_populations(pops, obs_c, loc_c)

  # Basis specification matching standard imuGAP bspline setup
  degree <- min(3L, max(1L, C - 1L))
  bsp <- if (C <= 3L) {
    splines::bs(seq_len(C), degree = degree, intercept = TRUE)
  } else {
    knots <- seq(1, C, length.out = max(3L, as.integer(C / 4L)))
    splines::bs(
      seq_len(C),
      knots = knots[-c(1, length(knots))],
      degree = degree,
      intercept = TRUE
    )
  }

  dose_sched <- sort(unique(wts$dose))
  sched <- imuGAP:::build_interval_schedule(dose_sched, wts$age)

  st_uncensored <- imuGAP:::slice_weights(
    wts,
    obs_c[is.na(censored)],
    "uncensored"
  )
  st_right <- imuGAP:::slice_weights(wts, obs_c[censored == 1], "right")
  st_left <- imuGAP:::slice_weights(wts, obs_c[0], "left")

  dat_stan <- c(
    list(
      n_yr = max(wts$age),
      n_cohort = max(wts$cohort)
    ),
    layer_data,
    list(
      n_doses = length(dose_sched),
      n_intervals = sched$n_intervals,
      dt_vec = sched$dt_vec,
      dose_sched = sched$dose_sched,
      age_to_interval_map = sched$age_to_interval_map,
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

  list(dat_stan = dat_stan, bsp = bsp)
}

# --- Core Evaluation Engine ---------------------------------------------------

#' Run inference benchmarking across configuration datasets and models
run_benchmark_inference <- function(
  config_input,
  output_rds = NULL,
  chains = 2L,
  iter = 300L,
  warmup = 150L,
  cores = 2L,
  seed = NULL,
  unified = TRUE
) {
  # 1. Resolve population datasets
  if (is.character(config_input) && grepl("\\.yml$", config_input)) {
    rds_pop_path <- sub("\\.yml$", ".rds", config_input)
    if (!file.exists(rds_pop_path)) {
      message(sprintf(
        "Population file '%s' not found. Generating...",
        rds_pop_path
      ))
      system2("Rscript", c("benchmark_synthetic_populations.R", config_input))
    }
    pop_data <- readRDS(rds_pop_path)
    cfg <- yaml::read_yaml(config_input)
  } else if (is.character(config_input) && grepl("\\.rds$", config_input)) {
    pop_data <- readRDS(config_input)
    cfg <- pop_data$config
  } else {
    stop("config_input must be a path to a .yml or .rds configuration file")
  }

  datasets <- pop_data$datasets
  n_datasets <- length(datasets)
  base_seed <- if (!is.null(seed)) {
    as.integer(seed)
  } else {
    (cfg$seed %||% 20261008L)
  }

  # 2. Resolve model combinations
  models <- get_valid_model_combinations(cfg)
  n_models <- length(models)

  message(sprintf(
    "Starting benchmark (unified=%s): %d datasets x %d models = %d total inference runs",
    unified,
    n_datasets,
    n_models,
    n_datasets * n_models
  ))

  # Preload unified Stan model objects if unified mode is active
  unified_cache <- list()
  if (isTRUE(unified)) {
    links <- unique(vapply(models, function(m) m$link, character(1)))
    for (lnk in links) {
      unified_cache[[lnk]] <- get_or_compile_unified_model(lnk)
    }
  }

  results <- list()
  run_idx <- 0L
  total_runs <- n_datasets * n_models

  for (m_info in models) {
    m_link <- m_info$link
    m_guess <- m_info$guess
    m_guess_type <- m_info$guess_type
    m_solver <- m_info$solver
    m_solver_type <- m_info$solver_type
    m_name <- m_info$model_name
    m_label <- m_info$label

    # Filter datasets matching the model link function
    link_datasets <- datasets[vapply(
      datasets,
      function(d) d$link == m_link,
      logical(1)
    )]
    if (length(link_datasets) == 0L) {
      next
    }

    # Load Stan model object (unified or standalone)
    stan_model_obj <- if (isTRUE(unified)) {
      unified_cache[[m_link]]
    } else {
      get_or_compile_cached_model(m_link, m_guess, m_solver)
    }

    for (ds in link_datasets) {
      run_idx <- run_idx + 1L
      t_run_start <- proc.time()

      message(
        sprintf(
          "[%d/%d] Model '%s' | K=%d | sigma=%.1f | sample=%d ... ",
          run_idx,
          total_runs,
          m_name,
          ds$K,
          ds$sigma,
          ds$sample_id
        ),
        appendLF = FALSE
      )

      prep <- prepare_benchmark_stan_data(
        ds,
        guess_type = m_guess_type,
        solver_type = m_solver_type
      )
      dat_stan <- prep$dat_stan
      bsp <- prep$bsp
      init_fn <- imuGAP:::make_init_fn(dat_stan, model = m_link)
      run_seed <- base_seed + ds$dataset_id * 100L + run_idx

      # Fit model with MCMC
      fit <- tryCatch(
        suppressWarnings(rstan::sampling(
          stan_model_obj,
          data = dat_stan,
          chains = chains,
          iter = iter,
          warmup = warmup,
          cores = cores,
          init = init_fn,
          seed = run_seed,
          refresh = 0,
          show_messages = FALSE
        )),
        error = function(e) e
      )

      t_elapsed <- (proc.time() - t_run_start)[["elapsed"]]

      if (inherits(fit, "error")) {
        message(sprintf("FAILED (%s)", conditionMessage(fit)))
        results[[length(results) + 1L]] <- data.table(
          dataset_id = ds$dataset_id,
          K = ds$K,
          sigma = ds$sigma,
          link = m_link,
          guess = m_guess,
          solver = m_solver,
          model_name = m_name,
          sample_id = ds$sample_id,
          elapsed_sec = t_elapsed,
          status = "error",
          error_msg = conditionMessage(fit),
          p0_link_mae = NA_real_,
          p0_js_dist = NA_real_,
          p0_coverage_95 = NA_real_,
          delta_link_mae = NA_real_,
          delta_coverage_95 = NA_real_,
          min_ess = NA_real_,
          mean_ess = NA_real_,
          min_ess_per_sec = NA_real_,
          max_rhat = NA_real_,
          n_divergent = NA_integer_
        )
        next
      }

      # Posterior summary & draws extraction
      fit_sum <- rstan::summary(fit)$summary
      draws <- rstan::extract(fit)

      # 1. p0 Root Trend Recovery
      beta_draws <- draws$beta_bs
      inv_fn <- if (m_link == "logit") stats::plogis else stats::pnorm
      link_fn <- if (m_link == "logit") stats::qlogis else stats::qnorm
      eta_draws <- beta_draws %*% t(bsp)
      p0_draws <- inv_fn(eta_draws)

      p0_mean <- colMeans(p0_draws)
      eta_mean <- colMeans(eta_draws)
      p0_q025 <- apply(p0_draws, 2L, stats::quantile, probs = 0.025)
      p0_q975 <- apply(p0_draws, 2L, stats::quantile, probs = 0.975)

      p0_true <- ds$p0_true
      eta_true <- link_fn(p0_true)

      p0_link_mae <- mean(abs(eta_mean - eta_true))
      p0_js_dist <- compute_bernoulli_jsd(p0_true, p0_mean)
      p0_cov <- mean(p0_true >= p0_q025 & p0_true <= p0_q975)

      # 2. Subpopulation Offset Recovery (from transformed parameter off_layer)
      off_draws <- draws$off_layer
      off_mean <- colMeans(off_draws)
      off_q025 <- apply(off_draws, 2L, stats::quantile, probs = 0.025)
      off_q975 <- apply(off_draws, 2L, stats::quantile, probs = 0.975)

      delta_true <- ds$delta_true
      delta_link_mae <- mean(abs(off_mean - delta_true))
      delta_cov <- mean(delta_true >= off_q025 & delta_true <= off_q975)

      # 3. MCMC Diagnostics
      n_eff_vals <- fit_sum[, "n_eff"]
      n_eff_clean <- n_eff_vals[
        !is.na(n_eff_vals) & is.finite(n_eff_vals) & n_eff_vals > 0
      ]
      min_ess <- if (length(n_eff_clean) > 0L) min(n_eff_clean) else NA_real_
      mean_ess <- if (length(n_eff_clean) > 0L) mean(n_eff_clean) else NA_real_
      min_ess_sec <- if (!is.na(min_ess) && t_elapsed > 0) {
        min_ess / t_elapsed
      } else {
        NA_real_
      }

      rhat_vals <- fit_sum[, "Rhat"]
      rhat_clean <- rhat_vals[!is.na(rhat_vals) & is.finite(rhat_vals)]
      max_rhat <- if (length(rhat_clean) > 0L) max(rhat_clean) else NA_real_
      n_div <- get_divergences(fit)

      message(sprintf(
        "DONE (%.1fs | p0_MAE=%.4f | JSD=%.4f | off_MAE=%.4f | min_ESS=%.0f | max_Rhat=%.3f | div=%d)",
        t_elapsed,
        p0_link_mae,
        p0_js_dist,
        delta_link_mae,
        min_ess,
        max_rhat,
        n_div
      ))

      results[[length(results) + 1L]] <- data.table(
        dataset_id = ds$dataset_id,
        K = ds$K,
        sigma = ds$sigma,
        link = m_link,
        guess = m_guess,
        solver = m_solver,
        model_name = m_name,
        sample_id = ds$sample_id,
        elapsed_sec = t_elapsed,
        status = "ok",
        error_msg = NA_character_,
        p0_link_mae = p0_link_mae,
        p0_js_dist = p0_js_dist,
        p0_coverage_95 = p0_cov,
        delta_link_mae = delta_link_mae,
        delta_coverage_95 = delta_cov,
        min_ess = min_ess,
        mean_ess = mean_ess,
        min_ess_per_sec = min_ess_sec,
        max_rhat = max_rhat,
        n_divergent = n_div
      )
    }
  }

  results_dt <- rbindlist(results)

  # Save results
  if (!is.null(output_rds)) {
    saveRDS(results_dt, file = output_rds)
    message(sprintf("Benchmark results saved to '%s'", output_rds))
  }

  invisible(results_dt)
}

# --- CLI Dispatch -------------------------------------------------------------

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0L) {
    cat(
      "Usage: Rscript benchmark_inference_runner.R <config.yml|config.rds> [output_results.rds] [--options]\n"
    )
    q(status = 1L)
  }

  config_arg <- args[1L]
  output_arg <- if (length(args) >= 2L && !grepl("^--", args[2L])) {
    args[2L]
  } else {
    cfg_base <- tools::file_path_sans_ext(basename(config_arg))
    sprintf("results_%s.rds", sub("^config_", "", cfg_base))
  }

  # Parse optional flags
  get_opt <- function(name, default_val) {
    pattern <- sprintf("^--%s=", name)
    match <- grep(pattern, args, value = TRUE)
    if (length(match) > 0L) {
      val <- sub(pattern, "", match[length(match)])
      as.integer(val)
    } else {
      default_val
    }
  }

  chains_opt <- get_opt("chains", 2L)
  iter_opt <- get_opt("iter", 300L)
  warmup_opt <- get_opt("warmup", 150L)
  cores_opt <- get_opt("cores", min(chains_opt, parallel::detectCores()))
  seed_opt <- get_opt("seed", NULL)
  unified_opt <- !("--standalone" %in% args)

  run_benchmark_inference(
    config_input = config_arg,
    output_rds = output_arg,
    chains = chains_opt,
    iter = iter_opt,
    warmup = warmup_opt,
    cores = cores_opt,
    seed = seed_opt,
    unified = unified_opt
  )
}
