#' Benchmark Inference Runner for imuGAP Solvers & Initial Guesses
#'
#' Evaluates MCMC inference accuracy, coverage, and sampling efficiency across
#' synthetic population datasets and compiled Stan models specified in a configuration.
#'
#' Usage:
#'   Rscript benchmark_inference_runner.R <config.yml | config.rds> [output.rds] [options]
#'
#' Options:
#'   --chains=N     Number of MCMC chains (default: 4)
#'   --iter=N       Total iterations per chain (default: 300)
#'   --warmup=N     Warmup iterations per chain (default: 150)
#'   --cores=N      Parallel CPU cores for sampling (default: min(chains, available))
#'   --seed=N       Base PRNG seed (default: from config or 20261008)
#'   --model-idx=N  Execute only model index N (1-indexed, for SLURM array tasks)
#'   --chunk-id=N   Execute chunk N of partitioned model set
#'   --n-chunks=N   Total number of chunks

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

#' Compute Winkler Interval Score for (1 - alpha) Credible Interval
compute_winkler_score <- function(lower, upper, true_val, alpha = 0.05) {
  width <- upper - lower
  pen_lower <- (2 / alpha) * pmax(0, lower - true_val)
  pen_upper <- (2 / alpha) * pmax(0, true_val - upper)
  mean(width + pen_lower + pen_upper)
}

#' Extract MCMC divergence count across chains
get_divergences <- function(fit) {
  sampler_params <- rstan::get_sampler_params(fit, inc_warmup = FALSE)
  sum(vapply(sampler_params, function(x) sum(x[, "divergent__"]), numeric(1)))
}

#' Prepare Stan input data list for 2-layer benchmark dataset
prepare_benchmark_stan_data <- function(
  ds,
  guess_type = 3L,
  solver_type = 1L,
  num_threads = 1L
) {
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
      num_threads = as.integer(num_threads)
    )
  )

  list(dat_stan = dat_stan, bsp = bsp)
}

# --- Core Evaluation Engine ---------------------------------------------------

#' Run inference benchmarking across configuration datasets and models
run_benchmark_inference <- function(
  config_input,
  output_rds = NULL,
  chains = 4L,
  iter = 300L,
  warmup = 150L,
  cores = 4L,
  threads_per_chain = 1L,
  seed = NULL,
  unified = TRUE,
  model_idx = NULL,
  chunk_id = NULL,
  n_chunks = NULL
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
  all_models <- get_valid_model_combinations(cfg)
  n_total_models <- length(all_models)

  # Model chunking / slicing
  if (!is.null(model_idx)) {
    if (model_idx < 1L || model_idx > n_total_models) {
      stop(sprintf(
        "model_idx %d out of bounds (valid range: 1..%d)",
        model_idx,
        n_total_models
      ))
    }
    models <- all_models[model_idx]
    message(sprintf(
      "Worker partition: running single model index %d/%d ('%s')",
      model_idx,
      n_total_models,
      models[[1L]]$model_name
    ))
  } else if (!is.null(chunk_id) && !is.null(n_chunks)) {
    if (chunk_id < 1L || chunk_id > n_chunks) {
      stop(sprintf("chunk_id %d out of bounds (1..%d)", chunk_id, n_chunks))
    }
    chunk_splits <- split(
      seq_len(n_total_models),
      cut(seq_len(n_total_models), n_chunks, labels = FALSE)
    )
    m_indices <- chunk_splits[[chunk_id]]
    models <- all_models[m_indices]
    message(sprintf(
      "Worker partition: running chunk %d/%d (models %s)",
      chunk_id,
      n_chunks,
      paste(m_indices, collapse = ", ")
    ))
  } else {
    models <- all_models
  }

  n_models <- length(models)

  # Compute exact total runs across matching link datasets
  total_runs <- sum(vapply(
    models,
    function(m) sum(vapply(datasets, function(d) d$link == m$link, logical(1))),
    integer(1)
  ))

  message(sprintf(
    "Starting benchmark (unified=%s, chains=%d, cores=%d): %d datasets, %d models -> %d total runs",
    unified,
    chains,
    cores,
    n_datasets,
    n_models,
    total_runs
  ))

  # Preload unified Stan model objects if unified mode is active
  unified_cache <- list()
  if (isTRUE(unified)) {
    links <- unique(vapply(models, function(m) m$link, character(1)))
    for (lnk in links) {
      unified_cache[[lnk]] <- get_or_compile_unified_model(lnk)
    }
  }

  results_runs <- list()
  results_p0 <- list()
  results_delta <- list()
  results_params <- list()
  completed_keys <- character()

  if (!is.null(output_rds) && file.exists(output_rds)) {
    prev_bundle <- tryCatch(readRDS(output_rds), error = function(e) NULL)
    if (
      is.list(prev_bundle) &&
        "runs" %in% names(prev_bundle) &&
        is.data.frame(prev_bundle$runs)
    ) {
      prev_runs <- prev_bundle$runs
      results_runs[[1L]] <- as.data.table(prev_runs)
      if ("p0_profiles" %in% names(prev_bundle)) {
        results_p0[[1L]] <- as.data.table(prev_bundle$p0_profiles)
      }
      if ("delta_profiles" %in% names(prev_bundle)) {
        results_delta[[1L]] <- as.data.table(prev_bundle$delta_profiles)
      }
      if ("param_summaries" %in% names(prev_bundle)) {
        results_params[[1L]] <- as.data.table(prev_bundle$param_summaries)
      }
      completed_keys <- paste(
        prev_runs$dataset_id,
        prev_runs$model_name,
        sep = "_"
      )
      message(sprintf(
        "Resuming benchmark: found %d completed fits in '%s' (skipping finished runs)",
        length(completed_keys),
        output_rds
      ))
    } else if (is.data.frame(prev_bundle) && nrow(prev_bundle) > 0L) {
      results_runs[[1L]] <- as.data.table(prev_bundle)
      completed_keys <- paste(
        prev_bundle$dataset_id,
        prev_bundle$model_name,
        sep = "_"
      )
    }
  }

  run_idx <- 0L
  t_suite_start <- proc.time()

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
      run_key <- paste(ds$dataset_id, m_name, sep = "_")
      if (run_key %in% completed_keys) {
        next
      }

      t_run_start <- proc.time()
      pct_done <- (run_idx / total_runs) * 100

      message(
        sprintf(
          "[%d/%d (%.1f%%)] Model '%s' | K=%d | sigma=%.1f | sample=%d ... ",
          run_idx,
          total_runs,
          pct_done,
          m_name,
          ds$K,
          ds$sigma,
          ds$sample_id
        ),
        appendLF = FALSE
      )
      flush.console()

      prep <- prepare_benchmark_stan_data(
        ds,
        guess_type = m_guess_type,
        solver_type = m_solver_type,
        num_threads = threads_per_chain
      )
      dat_stan <- prep$dat_stan
      bsp <- prep$bsp
      init_fn <- imuGAP:::make_init_fn(dat_stan, model = m_link)
      run_seed <- base_seed + ds$dataset_id * 100L + run_idx

      Sys.setenv(STAN_NUM_THREADS = as.character(threads_per_chain))

      # Fit model with MCMC (parallel chains, with multithreaded within-chain reduction)
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
        results_runs[[length(results_runs) + 1L]] <- data.table(
          dataset_id = ds$dataset_id,
          K = ds$K,
          sigma = ds$sigma,
          link = m_link,
          guess = m_guess,
          solver = m_solver,
          model_name = m_name,
          sample_id = ds$sample_id,
          chains = chains,
          threads_per_chain = threads_per_chain,
          elapsed_sec = t_elapsed,
          warmup_sec = NA_real_,
          sampling_sec = NA_real_,
          status = "error",
          error_msg = conditionMessage(fit),
          p0_link_mae = NA_real_,
          p0_link_rmse = NA_real_,
          p0_js_dist = NA_real_,
          p0_coverage_95 = NA_real_,
          p0_coverage_50 = NA_real_,
          p0_winkler_95 = NA_real_,
          delta_link_mae = NA_real_,
          delta_link_rmse = NA_real_,
          delta_coverage_95 = NA_real_,
          delta_coverage_50 = NA_real_,
          delta_winkler_95 = NA_real_,
          min_ess = NA_real_,
          mean_ess = NA_real_,
          min_ess_per_sec = NA_real_,
          max_rhat = NA_real_,
          n_divergent = NA_integer_,
          stepsize = NA_real_
        )
        next
      }

      # Posterior summary & draws extraction
      fit_sum <- rstan::summary(fit)$summary
      draws <- rstan::extract(fit)
      elapsed_mat <- rstan::get_elapsed_time(fit)
      warmup_sec <- if (nrow(elapsed_mat) > 0L) {
        mean(elapsed_mat[, "warmup"])
      } else {
        NA_real_
      }
      sampling_sec <- if (nrow(elapsed_mat) > 0L) {
        mean(elapsed_mat[, "sample"])
      } else {
        NA_real_
      }

      # 1. p0 Root Trend Recovery
      beta_draws <- draws$beta_bs
      inv_fn <- if (m_link == "logit") stats::plogis else stats::pnorm
      link_fn <- if (m_link == "logit") stats::qlogis else stats::qnorm
      eta_draws <- beta_draws %*% t(bsp)
      p0_draws <- inv_fn(eta_draws)

      p0_mean <- colMeans(p0_draws)
      p0_sd <- apply(p0_draws, 2L, stats::sd)
      eta_mean <- colMeans(eta_draws)
      eta_sd <- apply(eta_draws, 2L, stats::sd)

      p0_q025 <- apply(p0_draws, 2L, stats::quantile, probs = 0.025)
      p0_q25 <- apply(p0_draws, 2L, stats::quantile, probs = 0.25)
      p0_q50 <- apply(p0_draws, 2L, stats::quantile, probs = 0.50)
      p0_q75 <- apply(p0_draws, 2L, stats::quantile, probs = 0.75)
      p0_q975 <- apply(p0_draws, 2L, stats::quantile, probs = 0.975)

      p0_true <- ds$p0_true
      eta_true <- link_fn(p0_true)

      p0_link_mae <- mean(abs(eta_mean - eta_true))
      p0_link_rmse <- sqrt(mean((eta_mean - eta_true)^2))
      p0_js_dist <- compute_bernoulli_jsd(p0_true, p0_mean)
      p0_cov_95 <- mean(p0_true >= p0_q025 & p0_true <= p0_q975)
      p0_cov_50 <- mean(p0_true >= p0_q25 & p0_true <= p0_q75)
      p0_winkler_95 <- compute_winkler_score(
        p0_q025,
        p0_q975,
        p0_true,
        alpha = 0.05
      )

      # Record cohort p0 profiles
      results_p0[[length(results_p0) + 1L]] <- data.table(
        dataset_id = ds$dataset_id,
        model_name = m_name,
        cohort = seq_along(p0_true),
        p0_true = p0_true,
        eta_true = eta_true,
        p0_mean = p0_mean,
        p0_sd = p0_sd,
        p0_q025 = p0_q025,
        p0_q25 = p0_q25,
        p0_q50 = p0_q50,
        p0_q75 = p0_q75,
        p0_q975 = p0_q975,
        eta_mean = eta_mean,
        eta_sd = eta_sd
      )

      # 2. Subpopulation Offset Recovery (from transformed parameter off_layer)
      off_draws <- draws$off_layer
      off_mean <- colMeans(off_draws)
      off_sd <- apply(off_draws, 2L, stats::sd)
      off_q025 <- apply(off_draws, 2L, stats::quantile, probs = 0.025)
      off_q25 <- apply(off_draws, 2L, stats::quantile, probs = 0.25)
      off_q50 <- apply(off_draws, 2L, stats::quantile, probs = 0.50)
      off_q75 <- apply(off_draws, 2L, stats::quantile, probs = 0.75)
      off_q975 <- apply(off_draws, 2L, stats::quantile, probs = 0.975)

      delta_true <- ds$delta_true
      delta_link_mae <- mean(abs(off_mean - delta_true))
      delta_link_rmse <- sqrt(mean((off_mean - delta_true)^2))
      delta_cov_95 <- mean(delta_true >= off_q025 & delta_true <= off_q975)
      delta_cov_50 <- mean(delta_true >= off_q25 & delta_true <= off_q75)
      delta_winkler_95 <- compute_winkler_score(
        off_q025,
        off_q975,
        delta_true,
        alpha = 0.05
      )

      # Record subpopulation delta profiles
      results_delta[[length(results_delta) + 1L]] <- data.table(
        dataset_id = ds$dataset_id,
        model_name = m_name,
        node_idx = seq_along(delta_true),
        delta_true = delta_true,
        delta_mean = off_mean,
        delta_sd = off_sd,
        delta_q025 = off_q025,
        delta_q25 = off_q25,
        delta_q50 = off_q50,
        delta_q75 = off_q75,
        delta_q975 = off_q975
      )

      # 3. Parameter Summaries (excluding lp__)
      param_names <- rownames(fit_sum)
      clean_p_mask <- param_names != "lp__"
      results_params[[length(results_params) + 1L]] <- data.table(
        dataset_id = ds$dataset_id,
        model_name = m_name,
        parameter = param_names[clean_p_mask],
        mean = fit_sum[clean_p_mask, "mean"],
        sd = fit_sum[clean_p_mask, "sd"],
        q025 = fit_sum[clean_p_mask, "2.5%"],
        q50 = fit_sum[clean_p_mask, "50%"],
        q975 = fit_sum[clean_p_mask, "97.5%"],
        n_eff = fit_sum[clean_p_mask, "n_eff"],
        Rhat = fit_sum[clean_p_mask, "Rhat"]
      )

      # 4. MCMC Diagnostics
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

      sampler_params <- rstan::get_sampler_params(fit, inc_warmup = FALSE)
      stepsize_mean <- mean(vapply(
        sampler_params,
        function(x) mean(x[, "stepsize__"]),
        numeric(1)
      ))

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

      results_runs[[length(results_runs) + 1L]] <- data.table(
        dataset_id = ds$dataset_id,
        K = ds$K,
        sigma = ds$sigma,
        link = m_link,
        guess = m_guess,
        solver = m_solver,
        model_name = m_name,
        sample_id = ds$sample_id,
        chains = chains,
        threads_per_chain = threads_per_chain,
        elapsed_sec = t_elapsed,
        warmup_sec = warmup_sec,
        sampling_sec = sampling_sec,
        status = "ok",
        error_msg = NA_character_,
        p0_link_mae = p0_link_mae,
        p0_link_rmse = p0_link_rmse,
        p0_js_dist = p0_js_dist,
        p0_coverage_95 = p0_cov_95,
        p0_coverage_50 = p0_cov_50,
        p0_winkler_95 = p0_winkler_95,
        delta_link_mae = delta_link_mae,
        delta_link_rmse = delta_link_rmse,
        delta_coverage_95 = delta_cov_95,
        delta_coverage_50 = delta_cov_50,
        delta_winkler_95 = delta_winkler_95,
        min_ess = min_ess,
        mean_ess = mean_ess,
        min_ess_per_sec = min_ess_sec,
        max_rhat = max_rhat,
        n_divergent = n_div,
        stepsize = stepsize_mean
      )

      # Periodic checkpoint saving every 50 runs or at completion
      if (
        !is.null(output_rds) && (run_idx %% 50L == 0L || run_idx == total_runs)
      ) {
        res_tmp_bundle <- list(
          runs = rbindlist(results_runs),
          p0_profiles = rbindlist(results_p0),
          delta_profiles = rbindlist(results_delta),
          param_summaries = rbindlist(results_params)
        )
        saveRDS(res_tmp_bundle, file = output_rds)
        t_now <- (proc.time() - t_suite_start)[["elapsed"]]
        runs_done <- max(run_idx - length(completed_keys), 1L)
        eta_min <- ((total_runs - run_idx) / runs_done) * (t_now / 60)
        message(sprintf(
          "--- [Checkpoint] Saved %d/%d fits (%.1f min elapsed, ~%.1f min remaining) to '%s' ---",
          nrow(res_tmp_bundle$runs),
          total_runs,
          t_now / 60,
          eta_min,
          output_rds
        ))
        flush.console()
      }
    }
  }

  res_bundle <- list(
    runs = rbindlist(results_runs),
    p0_profiles = rbindlist(results_p0),
    delta_profiles = rbindlist(results_delta),
    param_summaries = rbindlist(results_params)
  )

  # Save results bundle
  if (!is.null(output_rds)) {
    saveRDS(res_bundle, file = output_rds)
    message(sprintf("Benchmark results saved to '%s'", output_rds))
  }

  invisible(res_bundle)
}

# --- CLI Dispatch -------------------------------------------------------------

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0L) {
    cat(
      "Usage: Rscript benchmark_inference_runner.R <config.yml|config.rds> [output.rds] [--options]\n"
    )
    q(status = 1L)
  }

  config_arg <- args[1L]

  # Parse optional flags
  get_opt_int <- function(name, default_val = NULL) {
    pattern <- sprintf("^--%s=", name)
    match <- grep(pattern, args, value = TRUE)
    if (length(match) > 0L) {
      val <- sub(pattern, "", match[length(match)])
      as.integer(val)
    } else {
      default_val
    }
  }

  chains_opt <- get_opt_int("chains", 4L)
  iter_opt <- get_opt_int("iter", 300L)
  warmup_opt <- get_opt_int("warmup", 150L)
  cores_opt <- get_opt_int("cores", min(chains_opt, parallel::detectCores()))
  threads_opt <- get_opt_int("threads-per-chain", 1L)
  seed_opt <- get_opt_int("seed", NULL)
  model_idx_opt <- get_opt_int("model-idx", NULL)
  chunk_id_opt <- get_opt_int("chunk-id", NULL)
  n_chunks_opt <- get_opt_int("n-chunks", NULL)
  unified_opt <- !("--standalone" %in% args)

  output_arg <- if (length(args) >= 2L && !grepl("^--", args[2L])) {
    args[2L]
  } else {
    cfg_base <- tools::file_path_sans_ext(basename(config_arg))
    cfg_name <- sub("^config_", "", cfg_base)
    if (!is.null(model_idx_opt)) {
      dir.create("results_parts", showWarnings = FALSE, recursive = TRUE)
      sprintf("results_parts/raw_part_%s_%02d.rds", cfg_name, model_idx_opt)
    } else if (!is.null(chunk_id_opt)) {
      dir.create("results_parts", showWarnings = FALSE, recursive = TRUE)
      sprintf("results_parts/raw_part_%s_%02d.rds", cfg_name, chunk_id_opt)
    } else {
      sprintf("results_%s.rds", cfg_name)
    }
  }

  run_benchmark_inference(
    config_input = config_arg,
    output_rds = output_arg,
    chains = chains_opt,
    iter = iter_opt,
    warmup = warmup_opt,
    cores = cores_opt,
    threads_per_chain = threads_opt,
    seed = seed_opt,
    unified = unified_opt,
    model_idx = model_idx_opt,
    chunk_id = chunk_id_opt,
    n_chunks = n_chunks_opt
  )
}
