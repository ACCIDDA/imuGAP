# Synthetic scenarios with calendar-time variation in the uptake rate lambda.
#
# Purpose (see the "time-varying uptake" issue): generate data with a known
# disruption in vaccination timing, so we can (1) show how the current static-
# lambda model absorbs it, and (2) later confirm a dynamic-lambda model can
# recover it.
#
# Unlike data-raw/DATASET.R, this script does NOT call usethis::use_data() and
# does not touch data/. Outputs go to data-raw/scenarios/output/ (git-ignored).
#
# Run from the package root:
#   Rscript data-raw/scenarios/dynamic_lambda.R
#
# Calendar index: t = cohort + age_interval - 1, so t = 1 is cohort 1's first
# year of life. Observations only reach t ~ 33 (newest cohort 30 observed at
# ChildVaxView's max age 3), so disruptions placed after that are invisible to
# the data. Every scenario uses the same seeds, so the pre-drawn random values
# are identical and only lambda differs between scenarios.

library(data.table)

# Load the package's R code without compiling the Stan models. The simulator
# only needs plain-R helpers (create_observation_populations(), create_target()),
# so we source R/ directly instead of pkgload::load_code(), which fails on a
# fresh session in pkgload >= 1.3, or load_all(), which needs compiled Stan code.
# R/stanmodels.R is skipped because it loads the compiled modules.
r_files <- setdiff(
  list.files("R", pattern = "[.]R$", full.names = TRUE),
  file.path("R", "stanmodels.R")
)
for (f in r_files) {
  source(f, local = environment())
}

# dataset_helpers.R calls imuGAP:::fn(); point those at the functions sourced
# above so the script uses the code on this branch, not an installed copy.
helpers <- readLines(file.path("data-raw", "dataset_helpers.R"))
eval(parse(text = gsub("imuGAP:::", "", helpers, fixed = TRUE)))

out_dir <- file.path("data-raw", "scenarios", "output")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Scenario definitions -----------------------------------------------------

n_cal <- 30L + 33L - 1L # n_cohort + n_yr - 1, matching get_simulation_setup()
cal <- seq_len(n_cal)

# Dip then catch-up: uptake slows to 40% for two calendar years, then a
# catch-up year at 150%, then back to baseline. Placed mid-series so several
# cohorts are observed before, during, and after.
dip_mult <- rep(1, n_cal)
dip_mult[24:25] <- 0.4
dip_mult[26] <- 1.5

# Slow drift: uptake rate declines linearly to 60% of baseline by t = 33, then
# stays there.
drift_mult <- pmax(0.6, 1 - 0.4 * (cal - 1) / 32)

scenarios <- list(
  static = list(lambda_mult = 1, keep_streams = "all"),
  dip = list(lambda_mult = dip_mult, keep_streams = "all"),
  drift = list(lambda_mult = drift_mult, keep_streams = "all"),
  # Identifiability check: separating lambda from phi needs the same cohort
  # observed at several ages. Drop TeenVaxView (ages 14-18) and the county
  # 6th-grade survey (age 11), keeping ChildVaxView and kindergarten entry.
  dip_kg_only = list(lambda_mult = dip_mult, keep_streams = "early")
)

# ---- Helpers --------------------------------------------------------------------

#' Label each simulated observation with the stream it came from
label_streams <- function(observations, setup) {
  observations[,
    stream := fcase(
      loc_id %in% setup$school_names,
      "school_kg",
      loc_id %in% setup$county_names,
      "county_grade6",
      loc_id == "State" & age_min %in% c(2L, 3L),
      "cvv",
      loc_id == "State" & age_min == setup$sch_start,
      "skv",
      loc_id == "State" & age_min == min(setup$study_ages),
      "tvv"
    )
  ]
  stopifnot(!anyNA(observations$stream))
  observations
}

#' Keep only some observation streams, dropping matching population rows
filter_streams <- function(sim, keep_streams) {
  if (identical(keep_streams, "all")) {
    return(sim)
  }
  keep <- switch(
    keep_streams,
    early = c("cvv", "skv", "school_kg"),
    stop("unknown keep_streams: ", keep_streams)
  )
  sim$observations_sim <- sim$observations_sim[stream %in% keep]
  sim$populations_sim <- sim$populations_sim[
    obs_id %in% sim$observations_sim$obs_id
  ]
  sim
}

run_scenario <- function(name, spec) {
  setup <- get_simulation_setup(
    seed = 93254,
    sigma_sch = 0.8,
    sigma_cnty = 0.4,
    lambda_mult = spec$lambda_mult
  )
  latent <- generate_latent_current(setup)
  sim <- simulate_observations_from_latent(setup, latent, uncensored = FALSE)
  sim$observations_sim <- label_streams(sim$observations_sim, setup)
  sim <- filter_streams(sim, spec$keep_streams)

  # Truth needed for recovery checks later
  sim$truth <- list(
    lambda = setup$lambda,
    lambda_mult = spec$lambda_mult,
    lambda_cal = setup$lambda_cal,
    uptake_cohort = setup$cov,
    uptake_static = setup$cov_static,
    phi_state = latent$phi_st
  )
  sim$scenario <- name
  saveRDS(sim, file.path(out_dir, paste0(name, ".rds")))
  sim
}

# ---- Run ------------------------------------------------------------------------

results <- Map(run_scenario, names(scenarios), scenarios)

# Sanity check against the tracked package data. State-level streams (ChildVaxView,
# SchoolVaxView, TeenVaxView) depend only on phi_state and lambda and must match
# exactly. School and county streams currently differ from data/*.rda even on
# unmodified main (the latent school/county probabilities don't reproduce the
# tracked fixture), so those only produce a warning.
load(file.path("data", "observations_sim.rda"))
chk <- merge(
  results$static$observations_sim[, .(obs_id, stream, positive)],
  observations_sim[, .(obs_id, positive_pkg = positive)],
  by = "obs_id"
)
state_streams <- c("cvv", "skv", "tvv")
stopifnot(
  "static scenario does not reproduce state-level package observations" =
    chk[stream %in% state_streams, all(positive == positive_pkg)]
)
n_mismatch <- chk[!stream %in% state_streams, sum(positive != positive_pkg)]
if (n_mismatch > 0L) {
  warning(sprintf(
    "%d school/county observations differ from data/observations_sim.rda (known fixture drift)",
    n_mismatch
  ))
}

# ---- Quick look -----------------------------------------------------------------
# Observed state-level kindergarten (SKV) coverage by cohort: a timing dip shows
# up as a drop in the cohorts whose age-5 year falls in the dip.

skv <- rbindlist(lapply(results, function(r) {
  r$observations_sim[stream == "skv", .(
    scenario = r$scenario,
    cohort,
    coverage = positive / sample_n
  )]
}))

lambda_dt <- rbindlist(lapply(results, function(r) {
  data.table(
    scenario = r$scenario,
    calendar = cal,
    lambda_dose1 = r$truth$lambda_cal[, 1]
  )
}))

if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  p_lambda <- ggplot(lambda_dt[calendar <= 35]) +
    aes(calendar, lambda_dose1, color = scenario, linetype = scenario) +
    geom_step() +
    geom_vline(xintercept = 33, linetype = "dotted") +
    labs(
      x = "Calendar index (dotted: last observed year)",
      y = "True dose-1 uptake rate"
    ) +
    theme_bw()
  p_skv <- ggplot(skv) +
    aes(cohort, coverage, color = scenario, linetype = scenario) +
    geom_line() +
    geom_point(size = 0.8) +
    labs(x = "Birth cohort", y = "Observed state kindergarten 2-dose coverage") +
    theme_bw()
  ggsave(file.path(out_dir, "lambda_by_scenario.png"), p_lambda, width = 7, height = 4)
  ggsave(file.path(out_dir, "skv_by_scenario.png"), p_skv, width = 7, height = 4)
}

cat("Scenarios written to", out_dir, ":\n")
print(rbindlist(lapply(results, function(r) {
  data.table(
    scenario = r$scenario,
    n_obs = nrow(r$observations_sim),
    streams = paste(sort(unique(r$observations_sim$stream)), collapse = ", ")
  )
})))
