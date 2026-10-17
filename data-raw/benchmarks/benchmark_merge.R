#' Benchmark Results Consolidator & Reducer
#'
#' Gathers raw partition results from worker tasks (SLURM array jobs or local chunks),
#' audits chunk completeness, extracts detailed posterior fits, and computes
#' consolidated summary matrices, solver rankings, and calibration profiles.
#'
#' Usage:
#'   Rscript benchmark_merge.R [config_name] [parts_dir] [output_prefix]
#'
#' Arguments:
#'   config_name    Name of configuration (e.g. 'strenuous', 'stress_test', 'full')
#'   parts_dir      Directory containing raw_part_*.rds files (default: 'results_parts')
#'   output_prefix  Prefix for output files (default: 'results_<config_name>')

suppressPackageStartupMessages({
  library(data.table)
})

# --- Helper Functions ---------------------------------------------------------

#' Consolidate Benchmark Results from Partition Files
consolidate_benchmark_results <- function(
  config_name = "strenuous",
  parts_dir = "results_parts",
  output_prefix = NULL
) {
  if (is.null(output_prefix)) {
    output_prefix <- sprintf("results_%s", config_name)
  }

  if (!dir.exists(parts_dir)) {
    stop(sprintf("Parts directory '%s' does not exist.", parts_dir))
  }

  pattern <- sprintf("^raw_part_%s_.*\\.rds$", config_name)
  part_files <- sort(list.files(
    parts_dir,
    pattern = pattern,
    full.names = TRUE
  ))

  if (length(part_files) == 0L) {
    # Fallback to any raw_part_*.rds in directory
    part_files <- sort(list.files(
      parts_dir,
      pattern = "\\.rds$",
      full.names = TRUE
    ))
  }

  if (length(part_files) == 0L) {
    stop(sprintf(
      "No partition files found in '%s' matching '%s'",
      parts_dir,
      pattern
    ))
  }

  message(sprintf(
    "Consolidating %d partition files from '%s' for '%s'...",
    length(part_files),
    parts_dir,
    config_name
  ))

  runs_list <- list()
  p0_list <- list()
  delta_list <- list()
  param_list <- list()

  for (i in seq_along(part_files)) {
    fpath <- part_files[i]
    part_data <- tryCatch(readRDS(fpath), error = function(e) {
      warning(sprintf("Could not read '%s': %s", fpath, conditionMessage(e)))
      NULL
    })

    if (is.null(part_data)) {
      next
    }

    if (is.list(part_data) && "runs" %in% names(part_data)) {
      runs_list[[length(runs_list) + 1L]] <- as.data.table(part_data$runs)
      if (
        "p0_profiles" %in% names(part_data) && !is.null(part_data$p0_profiles)
      ) {
        p0_list[[length(p0_list) + 1L]] <- as.data.table(part_data$p0_profiles)
      }
      if (
        "delta_profiles" %in%
          names(part_data) &&
          !is.null(part_data$delta_profiles)
      ) {
        delta_list[[length(delta_list) + 1L]] <- as.data.table(
          part_data$delta_profiles
        )
      }
      if (
        "param_summaries" %in%
          names(part_data) &&
          !is.null(part_data$param_summaries)
      ) {
        param_list[[length(param_list) + 1L]] <- as.data.table(
          part_data$param_summaries
        )
      }
    } else if (is.data.frame(part_data)) {
      runs_list[[length(runs_list) + 1L]] <- as.data.table(part_data)
    }
  }

  runs_dt <- rbindlist(runs_list, use.names = TRUE, fill = TRUE)
  p0_dt <- if (length(p0_list) > 0L) {
    rbindlist(p0_list, use.names = TRUE, fill = TRUE)
  } else {
    NULL
  }
  delta_dt <- if (length(delta_list) > 0L) {
    rbindlist(delta_list, use.names = TRUE, fill = TRUE)
  } else {
    NULL
  }
  param_dt <- if (length(param_list) > 0L) {
    rbindlist(param_list, use.names = TRUE, fill = TRUE)
  } else {
    NULL
  }

  message(sprintf("Assembled %d total inference runs.", nrow(runs_dt)))

  # --- 1. Consolidated Performance Matrix -------------------------------------
  matrix_dt <- runs_dt[,
    .(
      n_runs = .N,
      n_success = sum(status == "ok"),
      pct_divergent_runs = round(100 * mean(n_divergent > 0, na.rm = TRUE), 2),
      mean_divergences = round(mean(n_divergent, na.rm = TRUE), 3),
      pct_converged = round(
        100 * mean(max_rhat <= 1.05 & min_ess >= 100, na.rm = TRUE),
        2
      ),
      median_elapsed_sec = round(median(elapsed_sec, na.rm = TRUE), 3),
      median_warmup_sec = round(median(warmup_sec, na.rm = TRUE), 3),
      median_sampling_sec = round(median(sampling_sec, na.rm = TRUE), 3),
      median_min_ess_sec = round(median(min_ess_per_sec, na.rm = TRUE), 2),
      iqr_min_ess_sec = round(IQR(min_ess_per_sec, na.rm = TRUE), 2),
      mean_p0_mae = round(mean(p0_link_mae, na.rm = TRUE), 4),
      median_p0_mae = round(median(p0_link_mae, na.rm = TRUE), 4),
      mean_p0_js_dist = round(mean(p0_js_dist, na.rm = TRUE), 4),
      coverage_p0_95 = round(mean(p0_coverage_95, na.rm = TRUE), 3),
      coverage_p0_50 = round(mean(p0_coverage_50, na.rm = TRUE), 3),
      mean_p0_winkler_95 = round(mean(p0_winkler_95, na.rm = TRUE), 4),
      mean_delta_mae = round(mean(delta_link_mae, na.rm = TRUE), 4),
      median_delta_mae = round(median(delta_link_mae, na.rm = TRUE), 4),
      coverage_delta_95 = round(mean(delta_coverage_95, na.rm = TRUE), 3),
      coverage_delta_50 = round(mean(delta_coverage_50, na.rm = TRUE), 3),
      mean_delta_winkler_95 = round(mean(delta_winkler_95, na.rm = TRUE), 4)
    ),
    by = .(link, guess, solver, model_name, K, sigma)
  ]

  setorder(matrix_dt, link, solver, guess, K, sigma)

  # --- 2. Solver Ranking & Pareto Frontier ------------------------------------
  ranking_dt <- runs_dt[,
    .(
      n_runs = .N,
      pct_success = round(100 * mean(status == "ok"), 1),
      pct_divergent = round(100 * mean(n_divergent > 0, na.rm = TRUE), 2),
      pct_converged = round(
        100 * mean(max_rhat <= 1.05 & min_ess >= 100, na.rm = TRUE),
        2
      ),
      median_min_ess_sec = round(median(min_ess_per_sec, na.rm = TRUE), 2),
      mean_min_ess_sec = round(mean(min_ess_per_sec, na.rm = TRUE), 2),
      median_elapsed_sec = round(median(elapsed_sec, na.rm = TRUE), 3),
      mean_p0_mae = round(mean(p0_link_mae, na.rm = TRUE), 4),
      mean_p0_js_dist = round(mean(p0_js_dist, na.rm = TRUE), 4),
      mean_delta_mae = round(mean(delta_link_mae, na.rm = TRUE), 4),
      coverage_p0_95 = round(mean(p0_coverage_95, na.rm = TRUE), 3),
      coverage_delta_95 = round(mean(delta_coverage_95, na.rm = TRUE), 3)
    ),
    by = .(link, solver, guess, model_name)
  ]

  setorder(ranking_dt, link, -median_min_ess_sec)

  # --- 3. Calibration Envelopes -----------------------------------------------
  cal_p0 <- if (!is.null(p0_dt)) {
    p0_dt[,
      .(
        bias = round(mean(p0_mean - p0_true, na.rm = TRUE), 5),
        rmse = round(sqrt(mean((p0_mean - p0_true)^2, na.rm = TRUE)), 5),
        cov_95 = round(
          mean(p0_true >= p0_q025 & p0_true <= p0_q975, na.rm = TRUE),
          3
        ),
        cov_50 = round(
          mean(p0_true >= p0_q25 & p0_true <= p0_q75, na.rm = TRUE),
          3
        )
      ),
      by = .(model_name, cohort)
    ]
  } else {
    NULL
  }

  cal_delta <- if (!is.null(delta_dt)) {
    delta_dt[,
      .(
        bias = round(mean(delta_mean - delta_true, na.rm = TRUE), 5),
        rmse = round(sqrt(mean((delta_mean - delta_true)^2, na.rm = TRUE)), 5),
        cov_95 = round(
          mean(
            delta_true >= delta_q025 & delta_true <= delta_q975,
            na.rm = TRUE
          ),
          3
        ),
        cov_50 = round(
          mean(delta_true >= delta_q25 & delta_true <= delta_q75, na.rm = TRUE),
          3
        )
      ),
      by = .(model_name, node_idx)
    ]
  } else {
    NULL
  }

  # --- 4. Write Output Artifacts ----------------------------------------------
  runs_out <- sprintf("%s_runs.rds", output_prefix)
  matrix_rds <- sprintf("%s_matrix.rds", output_prefix)
  matrix_csv <- sprintf("%s_matrix.csv", output_prefix)
  ranking_rds <- sprintf("%s_ranking.rds", output_prefix)
  ranking_csv <- sprintf("%s_ranking.csv", output_prefix)
  cal_rds <- sprintf("%s_calibration.rds", output_prefix)
  bundle_rds <- sprintf("%s.rds", output_prefix)

  saveRDS(runs_dt, file = runs_out)
  saveRDS(matrix_dt, file = matrix_rds)
  fwrite(matrix_dt, file = matrix_csv)
  saveRDS(ranking_dt, file = ranking_rds)
  fwrite(ranking_dt, file = ranking_csv)

  if (!is.null(cal_p0) || !is.null(cal_delta)) {
    saveRDS(list(p0 = cal_p0, delta = cal_delta), file = cal_rds)
  }

  # Master bundle for backward compatibility & easy reporting
  master_bundle <- list(
    matrix = matrix_dt,
    ranking = ranking_dt,
    runs = runs_dt,
    calibration = list(p0 = cal_p0, delta = cal_delta),
    param_summaries = param_dt
  )
  saveRDS(master_bundle, file = bundle_rds)

  message(sprintf("Consolidated results written successfully:"))
  message(sprintf(
    "  - Runs table       : %s (%d rows)",
    runs_out,
    nrow(runs_dt)
  ))
  message(sprintf(
    "  - Matrix table     : %s / %s (%d cells)",
    matrix_rds,
    matrix_csv,
    nrow(matrix_dt)
  ))
  message(sprintf(
    "  - Solver ranking   : %s / %s (%d models)",
    ranking_rds,
    ranking_csv,
    nrow(ranking_dt)
  ))
  message(sprintf("  - Calibration      : %s", cal_rds))
  if (!is.null(param_dt)) {
    message(sprintf(
      "  - Param summaries  : %d parameter records preserved",
      nrow(param_dt)
    ))
  }
  message(sprintf("  - Master bundle    : %s", bundle_rds))

  invisible(master_bundle)
}

# --- CLI Dispatch -------------------------------------------------------------

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  cfg_name <- if (length(args) >= 1L) {
    sub("^config_", "", tools::file_path_sans_ext(basename(args[1L])))
  } else {
    "strenuous"
  }
  parts_path <- if (length(args) >= 2L) args[2L] else "results_parts"
  out_pfx <- if (length(args) >= 3L) args[3L] else NULL

  consolidate_benchmark_results(
    config_name = cfg_name,
    parts_dir = parts_path,
    output_prefix = out_pfx
  )
}
