# Fit the current (static-lambda) imuGAP model to the dynamic-lambda scenarios.
#
# Step 2 of the "time-varying uptake" issue: show what the static model does
# when vaccination timing is disrupted. Expectation: it explains a timing dip as
# extra never-vaccinators, so recovered phi rises for the affected cohorts.
#
# Requires the scenario files from data-raw/scenarios/dynamic_lambda.R and a
# working Stan toolchain (this loads the package with compiled models).
#
# Run from the package root:
#   Rscript data-raw/scenarios/fit_static.R
# Optional arguments: scenario names to fit (default: all), e.g.
#   Rscript data-raw/scenarios/fit_static.R static dip
# Set IMUGAP_QUICK=1 for a short smoke-test run (few iterations, 2 chains).
# Set IMUGAP_REUSE_FITS=1 to reload saved fits and only redo summaries/plots.

pkgload::load_all(quiet = TRUE)
library(data.table)

scen_dir <- file.path("data-raw", "scenarios", "output")
out_dir <- file.path(scen_dir, "fits_static")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

all_scenarios <- sub("[.]rds$", "", list.files(scen_dir, pattern = "[.]rds$"))
stopifnot(
  "no scenario files found; run data-raw/scenarios/dynamic_lambda.R first" = length(
    all_scenarios
  ) >
    0L
)
args <- commandArgs(trailingOnly = TRUE)
scenarios <- if (length(args)) args else all_scenarios
stopifnot(
  "unknown scenario name" = all(scenarios %in% all_scenarios)
)

quick <- identical(Sys.getenv("IMUGAP_QUICK"), "1")
st_opts <- if (quick) {
  stan_options(
    iter = 200,
    chains = 2L,
    threading = TRUE,
    refresh = 50,
    seed = 1L
  )
} else {
  stan_options(
    iter = 1000,
    chains = 4L,
    threading = TRUE,
    refresh = 250,
    seed = 1L
  )
}
im_opts <- imugap_options() # current defaults: logit, df = 5, static lambda

# ---- Helpers --------------------------------------------------------------------

#' Posterior summary of state-level phi by cohort, from the root spline
state_phi_summary <- function(fit, cohorts) {
  raw <- extract_imugap(fit, pars = "raw_phi_root")$raw_phi_root
  phi <- inv_link_r(raw, im_opts$link)
  data.table(
    cohort = cohorts,
    q2_5 = apply(phi, 2L, stats::quantile, 0.025),
    q50 = apply(phi, 2L, stats::quantile, 0.5),
    q97_5 = apply(phi, 2L, stats::quantile, 0.975)
  )
}

inv_link_r <- function(x, link) {
  if (identical(link, "probit")) stats::pnorm(x) else stats::plogis(x)
}

#' Posterior coverage vs. truth for the state at selected ages and doses
state_coverage_check <- function(fit, truth, cohorts) {
  target <- create_target(
    location = "State",
    age = c(2L, 5L, 11L, 18L),
    cohort = cohorts,
    dose = c(1L, 2L),
    mode = "enumerate"
  )
  pred <- summary(predict(fit, target = target, posterior_size = NULL))
  pred[,
    truth := (1 - truth$phi_state[cohort]) *
      truth$uptake_cohort[cbind(cohort, age, dose)]
  ]
  pred[, calendar := cohort + age - 1L]
  pred[]
}

#' Standardized residuals at single-age, uncensored observations
observation_residuals <- function(fit, obs) {
  obs_r <- obs[is.na(age_max) & is.na(censored)]
  target <- create_target(
    location = obs_r$loc_id,
    age = obs_r$age_min,
    cohort = obs_r$cohort,
    dose = obs_r$dose,
    mode = "error"
  )
  pred <- summary(predict(fit, target = target, posterior_size = NULL))
  obs_r[, `:=`(
    pred = pred$q50,
    observed = positive / sample_n,
    calendar = cohort + age_min - 1L
  )]
  obs_r[, std_resid := (observed - pred) / sqrt(pred * (1 - pred) / sample_n)]
  obs_r[]
}

# ---- Fit ------------------------------------------------------------------------

results <- list()
for (nm in scenarios) {
  cat("\n==== Fitting scenario:", nm, "====\n")
  sim <- readRDS(file.path(scen_dir, paste0(nm, ".rds")))
  obs <- copy(sim$observations_sim)[, stream := NULL]
  pops <- sim$populations_sim

  fit_file <- file.path(out_dir, paste0(nm, "_fit.rds"))
  if (
    identical(Sys.getenv("IMUGAP_REUSE_FITS"), "1") && file.exists(fit_file)
  ) {
    cat("  reusing saved fit:", fit_file, "\n")
    fit <- readRDS(fit_file)
  } else {
    t0 <- proc.time()
    fit <- sampling(
      observations = obs,
      populations = pops,
      locations = sim$locations_sim,
      imugap_opts = im_opts,
      stan_opts = st_opts
    )
    cat(sprintf("  wall time: %.1f s\n", (proc.time() - t0)[["elapsed"]]))
    saveRDS(fit, fit_file)
  }

  # The model's spline indexes cohorts from min(cohort) in the populations
  cohorts <- seq(min(pops$cohort), max(pops$cohort))
  stopifnot(
    "expected model cohorts to start at 1" = cohorts[1L] == 1L,
    length(cohorts) <= length(sim$truth$phi_state)
  )

  lambda <- exp(extract_imugap(fit, pars = "lambda_raw")$lambda_raw)

  phi_dt <- state_phi_summary(fit, cohorts)[,
    `:=`(scenario = nm, truth = sim$truth$phi_state[cohort])
  ]
  cov_dt <- state_coverage_check(fit, sim$truth, cohorts)[, scenario := nm]
  resid_dt <- observation_residuals(fit, sim$observations_sim)[,
    scenario := nm
  ]
  lambda_dt <- data.table(
    scenario = nm,
    dose = seq_len(ncol(lambda)),
    q2_5 = apply(lambda, 2L, stats::quantile, 0.025),
    q50 = apply(lambda, 2L, stats::quantile, 0.5),
    q97_5 = apply(lambda, 2L, stats::quantile, 0.975),
    base_truth = sim$truth$lambda
  )

  res <- list(
    phi = phi_dt,
    coverage = cov_dt,
    resid = resid_dt,
    lambda = lambda_dt
  )
  saveRDS(res, file.path(out_dir, paste0(nm, "_summary.rds")))
  results[[nm]] <- res
}

# ---- Combine & plot ---------------------------------------------------------------

phi_all <- rbindlist(lapply(results, `[[`, "phi"))
cov_all <- rbindlist(lapply(results, `[[`, "coverage"))
resid_all <- rbindlist(lapply(results, `[[`, "resid"))
lambda_all <- rbindlist(lapply(results, `[[`, "lambda"))

cat("\nPosterior uptake rates (lambda) vs. base truth:\n")
print(lambda_all)

phi_all[, covered := truth >= q2_5 & truth <= q97_5]
cat("\nState phi: share of cohorts whose 95% interval covers the truth\n")
print(phi_all[,
  .(coverage_95 = mean(covered), max_abs_err = max(abs(q50 - truth))),
  by = scenario
])

if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  p_phi <- ggplot(phi_all) +
    aes(x = cohort) +
    geom_ribbon(aes(ymin = q2_5, ymax = q97_5), fill = "grey70", alpha = 0.5) +
    geom_line(aes(y = q50, color = "Posterior median")) +
    geom_line(aes(y = truth, color = "Truth"), linetype = "dashed") +
    facet_wrap(~scenario) +
    scale_color_manual(
      NULL,
      values = c("Posterior median" = "black", "Truth" = "firebrick")
    ) +
    labs(x = "Birth cohort", y = "State non-uptake (phi)") +
    theme_bw()

  p_cov <- ggplot(cov_all[dose == 2L & age %in% c(5L, 18L)]) +
    aes(x = cohort) +
    geom_ribbon(aes(ymin = q2_5, ymax = q97_5), fill = "grey70", alpha = 0.5) +
    geom_line(aes(y = q50, color = "Posterior median")) +
    geom_line(aes(y = truth, color = "Truth"), linetype = "dashed") +
    facet_grid(paste("age", age) ~ scenario) +
    scale_color_manual(
      NULL,
      values = c("Posterior median" = "black", "Truth" = "firebrick")
    ) +
    labs(x = "Birth cohort", y = "State 2-dose coverage") +
    theme_bw()

  # Timing diagnostic: if uptake varies in calendar time, residuals from
  # different ages line up by calendar year rather than by cohort.
  resid_cal <- resid_all[,
    .(mean_std_resid = mean(std_resid), n = .N),
    by = .(scenario, stream, age_min, calendar)
  ]
  p_resid <- ggplot(resid_cal) +
    aes(calendar, mean_std_resid, color = factor(age_min), shape = stream) +
    geom_hline(yintercept = 0, color = "grey50") +
    geom_point() +
    facet_wrap(~scenario) +
    labs(
      x = "Calendar index (cohort + age - 1)",
      y = "Mean standardized residual",
      color = "Age",
      shape = "Stream"
    ) +
    theme_bw()

  ggsave(file.path(out_dir, "phi_recovery.png"), p_phi, width = 9, height = 6)
  ggsave(
    file.path(out_dir, "coverage_recovery.png"),
    p_cov,
    width = 10,
    height = 5
  )
  ggsave(
    file.path(out_dir, "residuals_by_calendar.png"),
    p_resid,
    width = 9,
    height = 6
  )
}

cat("\nSummaries and plots written to", out_dir, "\n")
