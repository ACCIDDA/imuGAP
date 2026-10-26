#' Generate Conforming Mock Benchmark Results for Visualization Brainstorming
#'
#' Reads `config_stress_test.rds` and synthesizes realistic posterior summaries
#' conforming exactly to the schema produced by `benchmark_inference_runner.R`.
#' Output is saved to `results_stress_test_mock.rds` and `results_stress_test.rds`.

suppressPackageStartupMessages({
  library(data.table)
  library(pkgload)
})

generate_mock_stress_test_results <- function(
  config_rds = pkgload::package_file(
    "data-raw",
    "benchmarks",
    "config_stress_test.rds"
  ),
  output_rds = pkgload::package_file(
    "data-raw",
    "benchmarks",
    "results_stress_test.rds"
  ),
  seed = 20261025L
) {
  set.seed(seed)
  stopifnot(file.exists(config_rds))
  pop_data <- readRDS(config_rds)
  cfg <- pop_data$config
  datasets <- pop_data$datasets

  # Model combinations for stress-test: zero & taylor2 x direct & builtin
  models_grid <- expand.grid(
    link = cfg$links,
    guess = cfg$guessers,
    solver = cfg$solvers,
    stringsAsFactors = FALSE
  )
  models_grid$model_name <- sprintf(
    "model_%s_%s_%s",
    models_grid$link,
    models_grid$guess,
    models_grid$solver
  )
  models_grid$label <- sprintf(
    "%s %s (%s)",
    tools::toTitleCase(models_grid$link),
    tools::toTitleCase(models_grid$guess),
    tools::toTitleCase(models_grid$solver)
  )

  results_runs <- list()
  results_p0 <- list()
  results_delta <- list()
  results_params <- list()

  for (m_idx in seq_len(nrow(models_grid))) {
    m_info <- models_grid[m_idx, ]
    m_link <- m_info$link
    m_guess <- m_info$guess
    m_solver <- m_info$solver
    m_name <- m_info$model_name

    is_builtin <- (m_solver == "builtin")
    is_taylor2 <- (m_guess == "taylor2")

    # Match datasets with model link
    matching_ds <- datasets[vapply(
      datasets,
      function(d) d$link == m_link,
      logical(1)
    )]

    for (ds in matching_ds) {
      K <- ds$K
      sigma <- ds$sigma
      delta_true <- ds$delta_true
      p0_true <- ds$p0_true
      ds_id <- ds$dataset_id

      # 1. Simulate Subpopulation Offsets delta_k
      # Builtin has near zero bias; Taylor2 direct has small bias; Zero direct has moderate bias
      if (is_builtin) {
        delta_bias <- stats::rnorm(K, mean = 0, sd = 0.005)
      } else if (is_taylor2) {
        delta_bias <- -0.025 *
          (sigma^2) *
          tanh(delta_true) +
          stats::rnorm(K, mean = 0, sd = 0.010)
      } else {
        delta_bias <- -0.120 *
          (sigma^2) *
          tanh(delta_true) +
          stats::rnorm(K, mean = 0, sd = 0.025)
      }

      off_sd <- pmax(
        0.03,
        0.04 + 0.06 * (sigma / sqrt(K)) + stats::rnorm(K, 0, 0.004)
      )
      off_mean <- delta_true + delta_bias + stats::rnorm(K, 0, off_sd / 6)
      off_q025 <- off_mean - 1.96 * off_sd
      off_q25 <- off_mean - 0.674 * off_sd
      off_q50 <- off_mean + stats::rnorm(K, 0, 0.003)
      off_q75 <- off_mean + 0.674 * off_sd
      off_q975 <- off_mean + 1.96 * off_sd

      delta_link_mae <- mean(abs(off_mean - delta_true))
      delta_link_rmse <- sqrt(mean((off_mean - delta_true)^2))
      delta_cov_95 <- mean(delta_true >= off_q025 & delta_true <= off_q975)
      delta_cov_50 <- mean(delta_true >= off_q25 & delta_true <= off_q75)
      delta_winkler_95 <- mean(
        (off_q975 - off_q025) +
          (2 / 0.05) * (off_q025 - delta_true) * (delta_true < off_q025) +
          (2 / 0.05) * (delta_true - off_q975) * (delta_true > off_q975)
      )

      results_delta[[length(results_delta) + 1L]] <- data.table(
        dataset_id = ds_id,
        model_name = m_name,
        node_idx = seq_len(K),
        delta_true = delta_true,
        delta_mean = off_mean,
        delta_sd = off_sd,
        delta_q025 = off_q025,
        delta_q25 = off_q25,
        delta_q50 = off_q50,
        delta_q75 = off_q75,
        delta_q975 = off_q975
      )

      # 2. Simulate Baseline Coverage p0
      g_fn <- if (m_link == "logit") stats::qlogis else stats::qnorm
      inv_g <- if (m_link == "logit") stats::plogis else stats::pnorm
      eta_true <- g_fn(p0_true)

      # Mean and sd on eta scale as a function of target parent probability p0
      # Curvature g''(p0) is zero at p0 = 0.5 and maximal near the tails (0.05, 0.95)
      curvature_factor <- (abs(p0_true - 0.5) * 2)^1.5
      eta_bias <- if (is_builtin) {
        stats::rnorm(length(p0_true), 0, 0.004)
      } else if (is_taylor2) {
        0.030 *
          (sigma^2) *
          curvature_factor +
          stats::rnorm(length(p0_true), 0, 0.008)
      } else {
        0.180 *
          (sigma^2) *
          curvature_factor +
          stats::rnorm(length(p0_true), 0, 0.018)
      }
      eta_sd <- pmax(
        0.02,
        0.03 +
          0.015 * (sigma / sqrt(K)) +
          stats::rnorm(length(p0_true), 0, 0.003)
      )
      eta_mean <- eta_true +
        eta_bias +
        stats::rnorm(length(p0_true), 0, eta_sd / 6)

      p0_mean <- inv_g(eta_mean)
      p0_sd <- p0_mean * (1 - p0_mean) * eta_sd
      p0_q025 <- inv_g(eta_mean - 1.96 * eta_sd)
      p0_q25 <- inv_g(eta_mean - 0.674 * eta_sd)
      p0_q50 <- inv_g(eta_mean)
      p0_q75 <- inv_g(eta_mean + 0.674 * eta_sd)
      p0_q975 <- inv_g(eta_mean + 1.96 * eta_sd)

      p0_link_mae <- mean(abs(eta_mean - eta_true))
      p0_link_rmse <- sqrt(mean((eta_mean - eta_true)^2))
      p0_cov_95 <- mean(p0_true >= p0_q025 & p0_true <= p0_q975)
      p0_cov_50 <- mean(p0_true >= p0_q25 & p0_true <= p0_q75)
      p0_winkler_95 <- mean(
        (p0_q975 - p0_q025) +
          (2 / 0.05) * (p0_q025 - p0_true) * (p0_true < p0_q025) +
          (2 / 0.05) * (p0_true - p0_q975) * (p0_true > p0_q975)
      )
      p0_js_dist <- mean(sqrt(abs(eta_mean - eta_true) * 0.05))

      results_p0[[length(results_p0) + 1L]] <- data.table(
        dataset_id = ds_id,
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

      # 3. Simulate Runtime & MCMC Diagnostics
      # Direct: fast ~1.5s - 2.5s; Builtin: slower ~4s - 20s (scales with K and sigma)
      if (is_builtin) {
        time_mult <- if (is_taylor2) 0.75 else 1.0
        elapsed_sec <- (3.5 + 0.6 * K + 3.0 * (sigma^1.5)) *
          time_mult +
          stats::rnorm(1, 0, 0.4)
      } else {
        time_mult <- if (is_taylor2) 1.05 else 1.0
        elapsed_sec <- (1.4 + 0.04 * K + 0.1 * sigma) *
          time_mult +
          stats::rnorm(1, 0, 0.1)
      }
      elapsed_sec <- max(0.5, round(elapsed_sec, 3))
      warmup_sec <- round(elapsed_sec * 0.46, 3)
      sampling_sec <- round(elapsed_sec * 0.54, 3)

      min_ess <- round(stats::rnorm(1, mean = 480, sd = 45))
      mean_ess <- round(min_ess * 1.55)
      min_ess_sec <- round(min_ess / elapsed_sec, 2)
      max_rhat <- round(1.00 + abs(stats::rnorm(1, 0, 0.007)), 3)
      n_div <- if (is_builtin && sigma > 1.0) stats::rpois(1, 0.1) else 0L

      results_runs[[length(results_runs) + 1L]] <- data.table(
        dataset_id = ds_id,
        K = K,
        sigma = sigma,
        link = m_link,
        guess = m_guess,
        solver = m_solver,
        model_name = m_name,
        sample_id = ds$sample_id,
        chains = 4L,
        threads_per_chain = 1L,
        elapsed_sec = elapsed_sec,
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
        stepsize = round(stats::rnorm(1, 0.045, 0.003), 4)
      )

      # 4. Parameter summaries placeholder
      results_params[[length(results_params) + 1L]] <- data.table(
        dataset_id = ds_id,
        model_name = m_name,
        parameter = c("sigma_delta", "bsp_sigma"),
        mean = c(
          sigma + stats::rnorm(1, 0, 0.02),
          0.5 + stats::rnorm(1, 0, 0.01)
        ),
        sd = c(0.04, 0.05),
        q025 = c(sigma - 0.08, 0.40),
        q50 = c(sigma, 0.50),
        q975 = c(sigma + 0.08, 0.60),
        n_eff = c(min_ess, mean_ess),
        Rhat = c(max_rhat, 1.00)
      )
    }
  }

  bundle <- list(
    runs = rbindlist(results_runs),
    p0_profiles = rbindlist(results_p0),
    delta_profiles = rbindlist(results_delta),
    param_summaries = rbindlist(results_params)
  )

  saveRDS(bundle, output_rds)
  message(sprintf(
    "Generated mock results bundle with %d runs across %d datasets -> saved to '%s'",
    nrow(bundle$runs),
    length(datasets),
    output_rds
  ))
  invisible(bundle)
}

if (sys.nframe() == 0L) {
  generate_mock_stress_test_results()
}
