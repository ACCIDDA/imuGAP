#' Benchmark Results Visualization Engine for imuGAP
#'
#' Generates publication-quality diagnostic and benchmarking figures:
#' 1. Parameter Recovery & Calibration (`fig_recovery_truth.png`)
#' 2. Error Behavior Across Strata as Function of p0 (`fig_error_strata.png`)
#' 3. Computational Runtime & Sampling Efficiency Across Strata (`fig_runtime_strata.png`)
#' 4. Pareto Accuracy vs Efficiency Frontier (`fig_pareto_tradeoff.png`)
#' 5. Direct Proposal Analytical Error Comparison (`fig_proposals_direct.png`)

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(scales)
  library(pkgload)
})

facet_nested_or_grid <- function(...) {
  if (requireNamespace("ggh4x", quietly = TRUE)) {
    ggh4x::facet_nested(...)
  } else {
    ggplot2::facet_grid(...)
  }
}

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "dataset_id",
    "model_name",
    "K",
    "sigma",
    "link",
    "guess",
    "solver",
    "delta_true",
    "delta_mean",
    "delta_q025",
    "delta_q975",
    "model_label",
    "guess_label",
    "solver_label",
    "stratum_label",
    "p0_true",
    "p0_mean",
    "prob_err",
    "k_label",
    "sigma_label",
    "link_label",
    "mae_median",
    "mae_q25",
    "mae_q75",
    "elapsed_sec",
    "delta_link_mae",
    "time_q25",
    "time_q75"
  ))
}

# --- Standard imuGAP Theme Setup ----------------------------------------------

imugap_benchmark_theme <- function(base_size = 11) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      legend.background = ggplot2::element_rect(fill = "white", colour = NA),
      legend.box.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_rect(fill = "white", colour = NA),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      text = ggplot2::element_text(colour = "black"),
      axis.text = ggplot2::element_text(colour = "black"),
      axis.title = ggplot2::element_text(colour = "black"),
      plot.title = ggplot2::element_text(
        colour = "black",
        face = "bold",
        size = 12
      ),
      plot.subtitle = ggplot2::element_text(colour = "black", size = 10),
      strip.text = ggplot2::element_text(colour = "black"),
      strip.background = ggplot2::element_rect(
        fill = "grey90",
        colour = "grey20"
      ),
      panel.border = ggplot2::element_rect(fill = NA, colour = "grey20"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(
        colour = "grey92",
        linewidth = 0.4
      )
    )
}

# Color palettes for proposal generators and solvers
GUESSER_PALETTE <- c(
  "Zero" = "#7F7F7F", # Neutral grey (baseline reference)
  "Taylor2" = "#1F78B4", # Muted blue (2nd-order expansion)
  "Taylor4" = "#33A02C", # Forest green (4th-order expansion)
  "Pade" = "#FF7F00", # Warm orange (Padé rational approximant)
  "Asymptotic" = "#E31A1C", # Vibrant red (tail asymptotic)
  "Conditioned" = "#6A3D9A" # Deep purple (piecewise hybrid dispatcher)
)

SOLVER_PALETTE <- c(
  "Direct" = "#1B9E77", # Dark2 teal (one-shot direct proposal)
  "Halley2" = "#D95F02", # Dark2 orange (2-iter Halley rational solver, default)
  "Newton2" = "#66A61E", # Dark2 olive green (2-iter Newton-Raphson)
  "Builtin" = "#7570B3", # Dark2 slate purple (Stan builtin algebraic solver)
  "Halley10" = "#E7298A" # Dark2 magenta (10-iter high-precision reference)
)

SOLVER_SHAPES <- c(
  "Direct" = 15, # Filled square
  "Halley2" = 16, # Filled circle
  "Newton2" = 18, # Filled diamond
  "Builtin" = 17, # Filled triangle
  "Halley10" = 8 # Asterisk
)

SOLVER_LINETYPES <- c(
  "Direct" = "dashed",
  "Halley2" = "solid",
  "Newton2" = "dotdash",
  "Builtin" = "dotted",
  "Halley10" = "longdash"
)

# Backwards-compatible MODEL_PALETTE mapping
MODEL_PALETTE <- c(
  "Zero (Direct)" = "#7F7F7F",
  "Zero (Halley2)" = "#D95F02",
  "Zero (Newton2)" = "#66A61E",
  "Zero (Builtin)" = "#7570B3",
  "Zero (Halley10)" = "#E7298A",
  "Taylor2 (Direct)" = "#1F78B4",
  "Taylor2 (Halley2)" = "#41B6C4",
  "Taylor2 (Builtin)" = "#225EA8",
  "Taylor4 (Direct)" = "#33A02C",
  "Taylor4 (Halley2)" = "#74C476",
  "Taylor4 (Builtin)" = "#006D2C",
  "Pade (Direct)" = "#FF7F00",
  "Pade (Halley2)" = "#FDBF6F",
  "Pade (Builtin)" = "#E65100",
  "Asymptotic (Direct)" = "#E31A1C",
  "Asymptotic (Halley2)" = "#FB9A99",
  "Asymptotic (Builtin)" = "#B10026",
  "Conditioned (Direct)" = "#6A3D9A",
  "Conditioned (Halley2)" = "#CAB2D6",
  "Conditioned (Builtin)" = "#4A148C"
)

# --- Figure 1: Parameter Recovery & Calibration -------------------------------

plot_recovery_truth <- function(bundle, output_file = NULL) {
  delta_dt <- data.table::as.data.table(bundle$delta_profiles)
  runs_dt <- data.table::as.data.table(bundle$runs)

  delta_meta <- unique(runs_dt[, list(
    dataset_id,
    model_name,
    K,
    sigma,
    link,
    guess,
    solver
  )])
  delta_plot <- merge(delta_dt, delta_meta, by = c("dataset_id", "model_name"))

  delta_plot[,
    guess_label := factor(
      tools::toTitleCase(guess),
      levels = names(GUESSER_PALETTE)
    )
  ]
  delta_plot[,
    solver_label := factor(
      tools::toTitleCase(solver),
      levels = names(SOLVER_PALETTE)
    )
  ]

  strata_dt <- unique(delta_plot[, .(sigma, K)])[order(sigma, K)]
  strata_lvls <- sprintf(
    "sigma[delta] == %0.1f ~ (K == %d)",
    strata_dt$sigma,
    strata_dt$K
  )
  delta_plot[,
    stratum_label := factor(
      sprintf("sigma[delta] == %0.1f ~ (K == %d)", sigma, K),
      levels = strata_lvls
    )
  ]

  # Sample subset for uncluttered visual scatter if dense
  sample_rows <- if (nrow(delta_plot) > 1200L) {
    sample(seq_len(nrow(delta_plot)), 1200L)
  } else {
    seq_len(nrow(delta_plot))
  }
  plot_sub <- delta_plot[sample_rows]

  p <- ggplot2::ggplot(
    plot_sub,
    ggplot2::aes(x = delta_true, y = delta_mean, color = solver_label)
  ) +
    ggplot2::geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed",
      color = "gray40",
      linewidth = 0.8
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = delta_q025, ymax = delta_q975),
      width = 0,
      alpha = 0.25,
      linewidth = 0.5
    ) +
    ggplot2::geom_point(alpha = 0.65, size = 1.3) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(stratum_label),
      cols = ggplot2::vars(guess_label),
      labeller = ggplot2::labeller(
        stratum_label = ggplot2::label_parsed,
        guess_label = ggplot2::label_value
      )
    ) +
    ggplot2::coord_cartesian(xlim = c(-3.6, 3.6), ylim = c(-3.6, 3.6)) +
    ggplot2::scale_color_manual(values = SOLVER_PALETTE, name = "Solver") +
    imugap_benchmark_theme() +
    ggplot2::labs(
      title = "Subpopulation Offset Parameter Recovery Across Strata",
      subtitle = "Posterior mean and 95% CI vs true offsets across initial proposals and solvers",
      x = "True Subpopulation Offset (delta_true)",
      y = "Posterior Mean Estimate (delta_mean)"
    )

  if (!is.null(output_file)) {
    dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
    ggplot2::ggsave(output_file, p, width = 12, height = 9, dpi = 300)
    message(sprintf("Saved: %s", output_file))
  }
  invisible(p)
}

# --- Figure 2: Error Behavior Across Strata (Link Scale MAE vs p0) ------------

plot_error_strata <- function(
  bundle,
  output_file = NULL,
  exclude_direct = TRUE
) {
  runs_dt <- data.table::as.data.table(bundle$runs)
  p0_dt <- data.table::as.data.table(bundle$p0_profiles)

  if (isTRUE(exclude_direct)) {
    runs_dt <- runs_dt[solver != "direct"]
  }

  meta <- unique(runs_dt[, list(
    dataset_id,
    model_name,
    K,
    sigma,
    link,
    guess,
    solver
  )])
  merged <- merge(p0_dt, meta, by = c("dataset_id", "model_name"))
  merged[, prob_err := abs(p0_mean - p0_true)]

  merged[,
    guess_label := factor(
      tools::toTitleCase(guess),
      levels = names(GUESSER_PALETTE)
    )
  ]
  active_solvers <- intersect(
    names(SOLVER_PALETTE),
    unique(tools::toTitleCase(merged$solver))
  )
  merged[,
    solver_label := factor(
      tools::toTitleCase(solver),
      levels = active_solvers
    )
  ]
  merged[,
    model_label := factor(
      sprintf("%s (%s)", tools::toTitleCase(guess), tools::toTitleCase(solver)),
      levels = unique(sprintf(
        "%s (%s)",
        tools::toTitleCase(guess),
        tools::toTitleCase(solver)
      ))
    )
  ]
  merged[,
    k_label := factor(
      sprintf("K == %d", K),
      levels = sprintf("K == %d", sort(unique(K)))
    )
  ]
  merged[,
    sigma_label := factor(
      sprintf("sigma[delta] == %0.1f", sigma),
      levels = sprintf("sigma[delta] == %0.1f", sort(unique(sigma)))
    )
  ]
  merged[,
    link_label := factor(
      sprintf("'%s Link'", tools::toTitleCase(link)),
      levels = c("'Logit Link'", "'Probit Link'")
    )
  ]

  agg <- merged[,
    list(
      mae_median = stats::median(prob_err, na.rm = TRUE),
      mae_q25 = stats::quantile(prob_err, 0.25, na.rm = TRUE),
      mae_q75 = stats::quantile(prob_err, 0.75, na.rm = TRUE),
      mae_mean = mean(prob_err, na.rm = TRUE)
    ),
    by = list(
      link_label,
      k_label,
      sigma_label,
      guess_label,
      solver_label,
      model_label,
      p0_true
    )
  ]

  agg[, `:=`(
    mae_q25 = pmax(mae_q25, 1e-6),
    mae_median = pmax(mae_median, 1e-6),
    mae_q75 = pmax(mae_q75, 1e-6)
  )]

  p_mae <- ggplot2::ggplot(
    agg,
    ggplot2::aes(
      x = p0_true,
      y = mae_median,
      color = guess_label,
      linetype = solver_label,
      group = model_label
    )
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = mae_q25, ymax = mae_q75, fill = guess_label),
      alpha = 0.12,
      color = NA
    ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(ggplot2::aes(shape = solver_label), size = 2.0) +
    facet_nested_or_grid(
      rows = ggplot2::vars(link_label),
      cols = ggplot2::vars(k_label, sigma_label),
      labeller = ggplot2::label_parsed
    ) +
    ggplot2::scale_color_manual(
      values = GUESSER_PALETTE,
      name = "Proposal Algorithm"
    ) +
    ggplot2::scale_fill_manual(
      values = GUESSER_PALETTE,
      name = "Proposal Algorithm"
    ) +
    ggplot2::scale_linetype_manual(values = SOLVER_LINETYPES, name = "Solver") +
    ggplot2::scale_shape_manual(values = SOLVER_SHAPES, name = "Solver") +
    ggplot2::scale_x_continuous(
      breaks = c(0.05, 0.50, 0.95),
      labels = scales::label_percent(accuracy = 1),
      minor_breaks = NULL
    ) +
    ggplot2::scale_y_log10(
      breaks = 10^seq(-5, -1, by = 1),
      labels = scales::label_log()
    ) +
    ggplot2::coord_cartesian(
      ylim = if (isTRUE(exclude_direct)) c(5e-6, 0.03) else c(2e-6, 0.2)
    ) +
    imugap_benchmark_theme() +
    ggplot2::labs(
      title = "Probability-Scale Absolute Error Across Strata (Log Scale)",
      subtitle = if (isTRUE(exclude_direct)) {
        paste(
          "Posterior median and IQR ribbon of |p0_hat - p0_true| across",
          "proposal algorithms (colors) and solved models (Halley2 vs Builtin, excluding direct)"
        )
      } else {
        paste(
          "Posterior median and IQR ribbon of |p0_hat - p0_true| across",
          "proposal algorithms (colors) and solvers (linetypes/shapes)"
        )
      },
      x = "Target Parent Probability (p0)",
      y = "Absolute Probability Error (|p0_hat - p0_true|, log10 scale)"
    )

  if (!is.null(output_file)) {
    dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
    ggplot2::ggsave(output_file, p_mae, width = 12, height = 8, dpi = 300)
    message(sprintf("Saved: %s", output_file))
  }
  invisible(p_mae)
}

# --- Figure 3: Computational Runtime & Sampling Efficiency Across Strata ------

plot_runtime_strata <- function(bundle, output_file = NULL) {
  runs_dt <- data.table::as.data.table(bundle$runs)

  runs_dt[,
    guess_label := factor(
      tools::toTitleCase(guess),
      levels = names(GUESSER_PALETTE)
    )
  ]
  runs_dt[,
    solver_label := factor(
      tools::toTitleCase(solver),
      levels = names(SOLVER_PALETTE)
    )
  ]
  runs_dt[,
    k_label := factor(
      sprintf("K == %d", K),
      levels = sprintf("K == %d", sort(unique(K)))
    )
  ]
  runs_dt[,
    sigma_label := factor(
      sprintf("sigma[delta] == %0.1f", sigma),
      levels = sprintf("sigma[delta] == %0.1f", sort(unique(sigma)))
    )
  ]
  runs_dt[,
    link_label := factor(
      sprintf("'%s Link'", tools::toTitleCase(link)),
      levels = c("'Logit Link'", "'Probit Link'")
    )
  ]

  p_time <- ggplot2::ggplot(
    runs_dt,
    ggplot2::aes(x = guess_label, y = elapsed_sec, fill = solver_label)
  ) +
    ggplot2::geom_boxplot(
      position = ggplot2::position_dodge(0.8),
      color = "grey20",
      linewidth = 0.45,
      outlier.size = 0.9,
      outlier.alpha = 0.4,
      width = 0.7
    ) +
    facet_nested_or_grid(
      rows = ggplot2::vars(link_label),
      cols = ggplot2::vars(k_label, sigma_label),
      labeller = ggplot2::label_parsed
    ) +
    ggplot2::scale_fill_manual(values = SOLVER_PALETTE, name = "Solver") +
    ggplot2::scale_y_log10(
      breaks = c(0.5, 1, 2, 5, 10, 20),
      labels = scales::label_number(suffix = "s")
    ) +
    ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 45)) +
    imugap_benchmark_theme() +
    ggplot2::labs(
      title = "MCMC Wall-Clock Sampling Duration Across Benchmarking Strata",
      subtitle = paste(
        "Elapsed sampling time across 4 chains on log-scale",
        "grouped by proposal algorithm and solver (lower is faster)"
      ),
      x = "Proposal Algorithm",
      y = "Elapsed Sampling Time (seconds, log-scale)"
    )

  if (!is.null(output_file)) {
    dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
    ggplot2::ggsave(output_file, p_time, width = 12, height = 7, dpi = 300)
    message(sprintf("Saved: %s", output_file))
  }
  invisible(p_time)
}

# --- Figure 4: Pareto Optimization (Accuracy vs Computational Efficiency) -----

plot_pareto_tradeoff <- function(bundle, output_file = NULL) {
  runs_dt <- data.table::as.data.table(bundle$runs)

  runs_dt[,
    guess_label := factor(
      tools::toTitleCase(guess),
      levels = names(GUESSER_PALETTE)
    )
  ]
  runs_dt[,
    solver_label := factor(
      tools::toTitleCase(solver),
      levels = names(SOLVER_PALETTE)
    )
  ]
  runs_dt[,
    model_label := factor(
      sprintf("%s (%s)", tools::toTitleCase(guess), tools::toTitleCase(solver)),
      levels = unique(sprintf(
        "%s (%s)",
        tools::toTitleCase(guess),
        tools::toTitleCase(solver)
      ))
    )
  ]
  runs_dt[,
    k_label := factor(
      sprintf("K == %d", K),
      levels = sprintf("K == %d", sort(unique(K)))
    )
  ]
  runs_dt[,
    sigma_label := factor(
      sprintf("sigma[delta] == %0.1f", sigma),
      levels = sprintf("sigma[delta] == %0.1f", sort(unique(sigma)))
    )
  ]
  runs_dt[,
    link_label := factor(
      sprintf("'%s Link'", tools::toTitleCase(link)),
      levels = c("'Logit Link'", "'Probit Link'")
    )
  ]

  agg_dt <- runs_dt[,
    list(
      median_mae = stats::median(delta_link_mae, na.rm = TRUE),
      median_time = stats::median(elapsed_sec, na.rm = TRUE),
      mae_q25 = stats::quantile(delta_link_mae, 0.25, na.rm = TRUE),
      mae_q75 = stats::quantile(delta_link_mae, 0.75, na.rm = TRUE),
      time_q25 = stats::quantile(elapsed_sec, 0.25, na.rm = TRUE),
      time_q75 = stats::quantile(elapsed_sec, 0.75, na.rm = TRUE)
    ),
    by = list(
      link_label,
      k_label,
      sigma_label,
      guess_label,
      solver_label,
      model_label
    )
  ]

  p_pareto <- ggplot2::ggplot(
    agg_dt,
    ggplot2::aes(
      x = median_time,
      y = median_mae,
      color = guess_label,
      shape = solver_label
    )
  ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mae_q25, ymax = mae_q75),
      width = 0,
      alpha = 0.5,
      linewidth = 0.5
    ) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = time_q25, xmax = time_q75),
      height = 0,
      alpha = 0.5,
      linewidth = 0.5
    ) +
    ggplot2::geom_point(size = 3.0, alpha = 0.95) +
    facet_nested_or_grid(
      rows = ggplot2::vars(link_label),
      cols = ggplot2::vars(k_label, sigma_label),
      labeller = ggplot2::label_parsed,
      scales = "free"
    ) +
    ggplot2::scale_color_manual(
      values = GUESSER_PALETTE,
      name = "Proposal Algorithm"
    ) +
    ggplot2::scale_shape_manual(values = SOLVER_SHAPES, name = "Solver") +
    ggplot2::scale_x_log10(labels = scales::label_number(suffix = "s")) +
    imugap_benchmark_theme() +
    ggplot2::labs(
      title = "Pareto Frontier: Inferential Error vs Computational Runtime",
      subtitle = "Median +/- IQR across replications (ideal is lower-left: fast and accurate)",
      x = "Median Elapsed Runtime (seconds, log-scale)",
      y = "Median Offset Link-Scale MAE"
    )

  if (!is.null(output_file)) {
    dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
    ggplot2::ggsave(output_file, p_pareto, width = 12, height = 7, dpi = 300)
    message(sprintf("Saved: %s", output_file))
  }
  invisible(p_pareto)
}

# --- Figure 5: Direct Proposal Evaluation Across Strata -----------------------

plot_proposals_direct <- function(bundle, output_file = NULL) {
  runs_dt <- data.table::as.data.table(bundle$runs)
  p0_dt <- data.table::as.data.table(bundle$p0_profiles)

  meta <- unique(runs_dt[
    solver == "direct",
    list(
      dataset_id,
      model_name,
      K,
      sigma,
      link,
      guess,
      solver
    )
  ])
  if (nrow(meta) == 0L) {
    message("plot_proposals_direct: no direct solver runs found in bundle.")
    return(invisible(NULL))
  }

  merged <- merge(p0_dt, meta, by = c("dataset_id", "model_name"))
  merged[, prob_err := abs(p0_mean - p0_true)]
  merged[,
    guess_label := factor(
      tools::toTitleCase(guess),
      levels = names(GUESSER_PALETTE)
    )
  ]
  merged[,
    k_label := factor(
      sprintf("K == %d", K),
      levels = sprintf("K == %d", sort(unique(K)))
    )
  ]
  merged[,
    sigma_label := factor(
      sprintf("sigma[delta] == %0.1f", sigma),
      levels = sprintf("sigma[delta] == %0.1f", sort(unique(sigma)))
    )
  ]
  merged[,
    link_label := factor(
      sprintf("'%s Link'", tools::toTitleCase(link)),
      levels = c("'Logit Link'", "'Probit Link'")
    )
  ]

  agg <- merged[,
    list(
      mae_median = stats::median(prob_err, na.rm = TRUE),
      mae_q25 = stats::quantile(prob_err, 0.25, na.rm = TRUE),
      mae_q75 = stats::quantile(prob_err, 0.75, na.rm = TRUE)
    ),
    by = list(link_label, k_label, sigma_label, guess_label, p0_true)
  ]

  agg[, `:=`(
    mae_q25 = pmax(mae_q25, 1e-6),
    mae_median = pmax(mae_median, 1e-6),
    mae_q75 = pmax(mae_q75, 1e-6)
  )]

  p <- ggplot2::ggplot(
    agg,
    ggplot2::aes(
      x = p0_true,
      y = mae_median,
      color = guess_label,
      group = guess_label
    )
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = mae_q25, ymax = mae_q75, fill = guess_label),
      alpha = 0.15,
      color = NA
    ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 2.4) +
    facet_nested_or_grid(
      rows = ggplot2::vars(link_label),
      cols = ggplot2::vars(k_label, sigma_label),
      labeller = ggplot2::label_parsed
    ) +
    ggplot2::scale_color_manual(
      values = GUESSER_PALETTE,
      name = "Proposal Algorithm"
    ) +
    ggplot2::scale_fill_manual(
      values = GUESSER_PALETTE,
      name = "Proposal Algorithm"
    ) +
    ggplot2::scale_x_continuous(
      breaks = c(0.05, 0.50, 0.95),
      labels = scales::label_percent(accuracy = 1),
      minor_breaks = NULL
    ) +
    ggplot2::scale_y_log10(
      breaks = 10^seq(-5, -1, by = 1),
      labels = scales::label_log()
    ) +
    ggplot2::coord_cartesian(ylim = c(2e-6, 0.2)) +
    imugap_benchmark_theme() +
    ggplot2::labs(
      title = "Initial Proposal Accuracy in One-Shot Direct Evaluation",
      subtitle = paste(
        "Absolute probability error |p0_hat - p0_true| with zero solver iterations",
        "(comparing analytical approximations directly across strata)"
      ),
      x = "Target Parent Probability (p0)",
      y = "Absolute Probability Error (log10 scale)"
    )

  if (!is.null(output_file)) {
    dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
    ggplot2::ggsave(output_file, p, width = 12, height = 7, dpi = 300)
    message(sprintf("Saved: %s", output_file))
  }
  invisible(p)
}

# --- Batch Figure Generator ---------------------------------------------------

generate_all_benchmark_figures <- function(
  results_rds = pkgload::package_file(
    "data-raw",
    "benchmarks",
    "results_stress_test.rds"
  ),
  out_dir = pkgload::package_file("data-raw", "benchmarks", "figures")
) {
  stopifnot(file.exists(results_rds))
  bundle <- readRDS(results_rds)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  f1 <- file.path(out_dir, "fig_recovery_truth.png")
  f2 <- file.path(out_dir, "fig_error_strata.png")
  f3 <- file.path(out_dir, "fig_runtime_strata.png")
  f4 <- file.path(out_dir, "fig_pareto_tradeoff.png")
  f5 <- file.path(out_dir, "fig_proposals_direct.png")

  plot_recovery_truth(bundle, f1)
  plot_error_strata(bundle, f2)
  plot_runtime_strata(bundle, f3)
  plot_pareto_tradeoff(bundle, f4)
  plot_proposals_direct(bundle, f5)

  cat("All benchmark figures successfully generated in:", out_dir, "\n")
  invisible(list(f1 = f1, f2 = f2, f3 = f3, f4 = f4, f5 = f5))
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  results_rds <- if (length(args) >= 1L && nzchar(args[1L])) {
    args[1L]
  } else {
    pkgload::package_file("data-raw", "benchmarks", "results_stress_test.rds")
  }
  out_target <- if (length(args) >= 2L && nzchar(args[2L])) {
    args[2L]
  } else {
    pkgload::package_file("data-raw", "benchmarks", "figures")
  }
  out_dir <- if (
    dir.exists(out_target) || !grepl("\\.[a-zA-Z0-9]+$", out_target)
  ) {
    out_target
  } else {
    dirname(out_target)
  }
  generate_all_benchmark_figures(results_rds = results_rds, out_dir = out_dir)
}
