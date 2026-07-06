# Part A of the package-data pipeline: simulate the *_sim inputs and the static
# latent-parameter fixture.
#
# This step depends on the private nc_measles dataset (read below) and so cannot
# run in CI; the resulting *_sim inputs and latent_params_sim are tracked in git.
# It also writes data-raw/sim_internals.rds, consumed by Part B
# (data-raw/fit_data.R) to build the genuinely fit-derived artifacts
# (fit_sim/target_sim/predict_sim) without re-running this simulation.
# latent_params_sim used to live in Part B, but its parameters and its
# (analytic, fit-free) coverage are simulation properties, not fit outputs, so
# it moved here as tracked static data (#105). Run with `just data` (or
# `just data-inputs` for this step alone).

# Load only the packages this script actually uses. If you attach e.g. the
# full tidyverse, you'll pull in lubridate; lubridate then gets captured in the
# fitted model's `@.MISC` environment and baked into data/fit_sim.rda, tripping
# R CMD check's "namespace references in data files".
library(data.table)

# use pkgload::load_code() to simulate having the contemporary version of imuGAP
# available for data generation
if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_code()
} else {
  stop("pkgload not found")
}

library(dplyr)

################################################################################
# A.1 Setup Structural & Latent Features #######################################
################################################################################

n_yr <- 33
n_cohort <- 30
ref_year <- 1995
phi_st <- c(
  0.8401733,
  0.8458791,
  0.8515769,
  0.8572586,
  0.8629160,
  0.8685411,
  0.8741259,
  0.8796623,
  0.8851422,
  0.8905575,
  0.8958959,
  0.9011275,
  0.9062182,
  0.9111339,
  0.9158404,
  0.9203035,
  0.9244892,
  0.9283632,
  0.9318916,
  0.9350400,
  0.9377351,
  0.9397467,
  0.9408054,
  0.9407130,
  0.9395576,
  0.9375024,
  0.9347246,
  0.9314054,
  0.9277256,
  0.9298663
)

stopifnot(
  "phi_st must have same length as # of cohorts" = length(phi_st) == n_cohort
)

# Setup a 2-dose vaccine, like MMR
lambda <- c(2.8, 3.0)
n_doses <- length(lambda)

# Setup the dose schedule
dose_schedule <- c(1, 4)
doses <- matrix(0, ncol = length(dose_schedule), nrow = n_yr)
for (i in seq_along(dose_schedule)) {
  doses[(dose_schedule[i] + 1):nrow(doses), i] <- 1
}

# Calculate the lambda-implied coverage
cov <- matrix(data = 0, nrow = n_yr, ncol = n_doses)

for (d in seq_len(n_doses)) {
  # setup eligibility for dose `d`
  ref <- if (d == 1L) {
    # if dose == 1L, everyone eligible
    rep(1, n_yr)
  } else {
    # if dose == d, everyone with dose d - 1
    cov[, d - 1L]
  }
  survival <- (1 - exp(-lambda[d] * doses[, d]))
  for (i in 2:n_yr) {
    cov[i, d] <- cov[i - 1, d] + (ref[i] - cov[i - 1, d]) * survival[i]
  }
}

# theme = NC musicians
county_names <- c("Scruggs", "Simone", "Watson")
# theme = NC native birds
school_names <- c(
  "Chickadee Elementary",
  "Nuthatch Academy",
  "Blue Heron School",
  "Flycatcher Elementary",
  "Bluebird Learning Center",
  "Catbird Academy",
  "Finch Elementary",
  "Sparrow School",
  "Towhee Children's Academy",
  "Warbler Elementary",
  "Egret Elementary",
  "Cardinal Academy",
  "Bunting School",
  "Tanager Academy",
  "Oriole Youth Academy",
  "Grosbeak Learning Center",
  "Junco Elementary",
  "Meadowlark School",
  "Goldfinch Elementary",
  "Mockingbird Academy",
  "Kinglet Learning Center",
  "Vireo School",
  "Kingfisher Academy",
  "Cormorant Elementary"
)

sch_per_cnty <- data.frame(parent_id = county_names, n_sch = c(10L, 7L, 7L))
tot_sch <- sum(sch_per_cnty$n_sch)

stopifnot(
  "School names length != school count" = length(school_names) == tot_sch
)

sigma_sch <- 0.8
sigma_cnty <- 0.4

other_vax_reduction <- 0.95

################################################################################
# A.2 Sample Latent Features ###################################################
################################################################################

set.seed(93254)

cnty_offset <- rnorm(length(county_names), 0, sigma_cnty)
names(cnty_offset) <- county_names
sch_offset <- rnorm(tot_sch, 0, sigma_sch)

################################################################################
# A.3 ChildVaxView #############################################################
################################################################################

# ChildVaxView occurs at the State level. For example purposes, we're treating
# the observations as if they are censored (i.e. minimum) levels of vaccination
n_cvv <- round(runif(n_cohort, 250, 450))
vax_inc <- cov[3, 1] - cov[2, 1]
at_24 <- rbinom(n_cohort, n_cvv, phi_st * cov[2, 1] * other_vax_reduction)
at_36 <- at_24 +
  rbinom(
    n_cohort,
    n_cvv - at_24,
    phi_st * vax_inc * other_vax_reduction
  )

sim_child <- rbind(
  data.frame(
    loc_id = "State",
    parent_id = NA_character_,
    year = seq_len(n_cohort) + ref_year,
    age_min = 2,
    positive = at_24,
    sample_n = n_cvv,
    dose = 1L
  ),
  data.frame(
    loc_id = "State",
    parent_id = NA_character_,
    year = seq_len(n_cohort) + ref_year,
    age_min = 3,
    positive = at_36,
    sample_n = n_cvv,
    dose = 1L
  )
)

sim_child$censored <- 1

# TeenVaxView occurs at the State level; we simulate an independent set of
# participants every year.
teen_yrs <- 18:30
study_ages <- 18:14

sim_teen <- data.frame(
  loc_id = "State",
  parent_id = NA_character_,
  year = teen_yrs + ref_year,
  positive = numeric(length(teen_yrs)),
  sample_n = numeric(length(teen_yrs)),
  age_min = min(study_ages),
  age_max = max(study_ages) + 1L
)

for (i in seq_len(nrow(sim_teen))) {
  samp_size <- as.integer(runif(length(study_ages), 40, 70))
  phi_slice <- teen_yrs[i] - study_ages + 1
  sim_teen$sample_n[i] <- sum(samp_size)
  sim_teen$positive[i] <- sum(
    rbinom(
      length(study_ages),
      samp_size,
      phi_st[phi_slice] * cov[study_ages, 2]
    )
  )
}

sim_teen$dose <- 2L

# Simulate detailed school-level data
sch_start <- 5L
sch_yrs <- (sch_start + 1L):30
nsch_base <- rlnorm(tot_sch, log(75), log(2.5))
# potentially resample to get a truncated lognormal
badindices <- which(!between(nsch_base, 10, 450))
while (length(badindices)) {
  nsch_base[badindices] <- rlnorm(length(badindices), log(75), log(2.5))
  badindices <- which(!between(nsch_base, 10, 450))
}
nsch_base <- as.integer(round(nsch_base))

kg_sim_full <- list()
cnty_ids <- with(sch_per_cnty, rep(parent_id, times = n_sch))
for (s in seq_len(tot_sch)) {
  nsch <- integer(length(sch_yrs))
  nsch[1] <- nsch_base[s]
  for (y in 2:length(sch_yrs)) {
    nsch[y] <- nsch[y - 1] + as.integer(round(5 * runif(1, min = -1, max = 1)))
    if (nsch[y] < 4) {
      nsch[y] <- 4L
    }
  }
  offset <- sch_offset[s] + cnty_offset[cnty_ids[s]]
  cov_temp <- plogis(qlogis(phi_st[sch_yrs - sch_start]) + offset) *
    cov[sch_start, 2]
  kg_sim_full[[s]] <- data.frame(
    year = sch_yrs + ref_year,
    parent_id = cnty_ids[s],
    loc_id = school_names[s],
    positive = rbinom(length(sch_yrs), nsch, cov_temp),
    sample_n = nsch
  )
}
kg_sim <- rbindlist(kg_sim_full)

# Aggregate school-data in SchoolVaxView
sim_school <- kg_sim[,
  {
    tot_vax <- sum(positive)
    .(
      tot_vax = tot_vax,
      tot_non = sum(sample_n) - tot_vax
    )
  },
  by = year
][, {
  npos <- rbinom(.N, tot_vax, 0.9)
  .(
    loc_id = "State",
    parent_id = NA_character_,
    sample_n = npos + rbinom(.N, tot_non, 0.9),
    positive = npos,
    year,
    age_min = sch_start,
    dose = 2L
  )
}]

# Bind vax view simulation together
vv_sim <- rbindlist(
  list(sim_child, sim_school, sim_teen),
  use.names = TRUE,
  fill = TRUE
)

# Assign age_min and dose to kg_sim
kg_sim$age_min <- 5
kg_sim$dose <- 2L

# Combine kg_sim and vv_sim
observations_sim <- rbindlist(
  list(kg_sim, vv_sim),
  use.names = TRUE,
  fill = TRUE
)

# Calculate normalized cohorts (using age_min and age_max)
observations_sim <- observations_sim |>
  mutate(
    age_max_val = ifelse(is.na(age_max), age_min + 1, age_max),
    by_min = year - age_max_val + 1,
    cohort_min = by_min - min(by_min) + 1
  ) |>
  dplyr::select(-by_min, -age_max_val)

# Assign sequential obs_id
observations_sim$obs_id <- seq_len(nrow(observations_sim))
observations_sim <- setDT(observations_sim)

# Create populations
obs_for_pop <- copy(observations_sim)
obs_for_pop[, cohort := cohort_min]

populations_sim <- imuGAP:::create_observation_populations(
  obs_for_pop,
  mode = "snapshot"
)

# Create locations mapping
locations_sim <- rbindlist(
  list(
    data.frame(loc_id = "State", parent_id = NA_character_),
    data.frame(loc_id = county_names, parent_id = "State"),
    unique(observations_sim[loc_id != "State", .(loc_id, parent_id)])
  ),
  use.names = TRUE,
  fill = TRUE
)

locations_sim <- setDT(locations_sim)

# Create imugap input package data objects
usethis::use_data(observations_sim, overwrite = TRUE)
usethis::use_data(populations_sim, overwrite = TRUE)
usethis::use_data(locations_sim, overwrite = TRUE)

# Persist the simulation internals that the fitted-data step needs.
#
# data-raw/fit_data.R rebuilds the fitted artifacts (fit_sim, target_sim,
# predict_sim, latent_params_sim) from the tracked *_sim inputs. The true
# (latent) coverage and the latent-parameter fixture depend on simulation
# internals that are not recoverable from those inputs alone -- the per-school
# and per-county offsets, the underlying coverage curves, etc. They are saved
# here so fit_data.R can run without re-running this nc_measles-dependent
# simulation. This file is build-ignored (see .Rbuildignore) but tracked in git.
sim_internals <- list(
  phi_st = phi_st,
  lambda = lambda,
  sigma_sch = sigma_sch,
  sigma_cnty = sigma_cnty,
  off_sch = sch_offset,
  off_cnty = cnty_offset,
  censor_reduction = other_vax_reduction,
  uptake = cov,
  county_names = county_names,
  school_names = school_names,
  cnty_ids = cnty_ids
)
saveRDS(sim_internals, "data-raw/sim_internals.rds")

# Latent parameters + true coverage (tracked static data, #105).
#
# latent_params_sim bundles the simulation's latent parameters together with
# `coverage`: the true/background coverage for each row of the prediction target
# grid. Neither depends on the Stan fit -- the parameters are the internals
# above, and coverage is derived analytically from them -- so now that
# create_target() builds the target grid without a fit (#110), this is computed
# here as tracked static data rather than regenerated on every build. Part B
# (data-raw/fit_data.R) rebuilds target_sim from the same create_target() grid
# spec and asserts the row counts still line up.
target_grid <- create_target(
  location = unique(locations_sim$loc_id),
  age = 1:18,
  cohort = max(populations_sim$cohort) - 18,
  dose = c(1, 2),
  mode = "snapshot"
)
target_grid <- data.table::as.data.table(target_grid)

coverage <- numeric(nrow(target_grid))
for (i in seq_len(nrow(target_grid))) {
  loc <- target_grid$loc_id[i]
  cohort_val <- target_grid$cohort[i]
  age_val <- target_grid$age[i]
  dose_val <- target_grid$dose[i]

  if (loc == "State") {
    offset <- 0
  } else if (loc %in% sim_internals$county_names) {
    c_idx <- match(loc, sim_internals$county_names)
    offset <- sim_internals$off_cnty[c_idx]
  } else {
    s_idx <- match(loc, sim_internals$school_names)
    offset <- sim_internals$off_sch[s_idx] +
      sim_internals$off_cnty[sim_internals$cnty_ids[s_idx]]
  }

  coverage[i] <- stats::plogis(
    stats::qlogis(sim_internals$phi_st[cohort_val]) + offset
  ) *
    sim_internals$uptake[age_val, dose_val]
}

latent_params_sim <- list(
  phi_state = sim_internals$phi_st,
  lambda = sim_internals$lambda,
  sigma_sch = sim_internals$sigma_sch,
  sigma_cnty = sim_internals$sigma_cnty,
  off_sch = sim_internals$off_sch,
  off_cnty = sim_internals$off_cnty,
  censor_reduction = sim_internals$censor_reduction,
  uptake = sim_internals$uptake,
  coverage = coverage
)
usethis::use_data(latent_params_sim, overwrite = TRUE)
