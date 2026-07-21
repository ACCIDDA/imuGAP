# Create the latent processes for the example data.
#
# Simulates time-varying state-level trends in vaccine propensity, with county-
# and school-level random effects.
#
# All elements are sampled as if odds-ratio factors against an indifferent
# baseline odds of 1.0.
library(data.table)

# uses pkgload::load_code() to load the contemporary version of imuGAP for data
# generation steps
if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_code() # n.b., if pkgload <= 1.5.3, may fail due a load_code bug
} else {
  stop("pkgload not found")
}

################################################################################
# A.1 Setup Structural & Latent Features #######################################
################################################################################

# going simulate out to age 33 for 30 birth cohorts
n_yr <- 33L
n_cohort <- 30L

# The final cohort corresponds to the youngest birth
# cohort observed in this year, which is ChildVaxView 3yo data in our example
# will use cohort := year - XYZ_offset + 1L to determine the cohort
cvv_max_age <- 3L
# final cohort + age must be a constant
# TeenVaxView goes out to 18 yo; so senior cohort + 18L == n_cohort + 3L
tvv_max_age <- 18L
tvv_min_age <- 14L
tvv_max_cohort <- n_cohort + cvv_max_age - tvv_max_age

tvv_cohorts <- seq_len(tvv_max_cohort)
study_ages <- tvv_max_age:tvv_min_age

# School Observations are for 5 yo; so K cohort + 5L == n_cohort + 3L
sch_start <- 5L
skv_max_cohort <- n_cohort + cvv_max_age - sch_start
skv_cohorts <- seq_len(skv_max_cohort)

# need to routinely switch back and forth betweens odds vs probs space
p_to_odds <- function(p) p / (1 - p)
odds_to_p <- function(odds) odds / (1 + odds)

phi_st_OR <- c(
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
) |>
  p_to_odds()

stopifnot(
  "phi_st must have same length as # of cohorts" = length(phi_st_OR) == n_cohort
)

# Setup a 2-dose vaccine, like MMR; lambda is the per-time constant hazard
lambda <- c(2.8, 3.0)
n_doses <- length(lambda)

# Setup the dose schedule, dose 1 eligibility at age 1, 2 at age 4
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

sch_per_cnty <- data.table(
  parent_id = county_names,
  n_sch = c(10L, 7L, 7L)
)[,
  ul := cumsum(n_sch)
][,
  ll := c(0L, head(ul, -1L)) + 1L
]
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

# Simulate detailed school-level enrollment
nsch_base <- rlnorm(tot_sch, log(75), log(2.5))
# potentially resample to get a truncated lognormal
badindices <- which(!between(nsch_base, 10, 450))
while (length(badindices)) {
  nsch_base[badindices] <- rlnorm(length(badindices), log(75), log(2.5))
  badindices <- which(!between(nsch_base, 10, 450))
}
nsch_base <- as.integer(round(nsch_base))
ncty_base <- sch_per_cnty[, mapply(function(l, u) sum(nsch_base[l:u]), ll, ul)]

# now sample the underlying offsets; constructed such that log odds ratios
# have a sampled mean of zero at each level. N.B. this implies that the OR
# effects balance to the enclosing effect, but *NOT* that probabilities average

draw_constrained_normal <- function(w, sigma) {
  n <- length(w)
  qr_decomp <- qr(matrix(w, ncol = 1))
  P <- qr.Q(qr_decomp, complete = TRUE)[, -1, drop = FALSE]
  z <- rnorm(n - 1)
  as.numeric((P %*% z) * sigma)
}

ncty_share <- ncty_base / sum(ncty_base)

sch_per_cnty[,
  cnty_OR := draw_constrained_normal(ncty_share, sigma_cnty) |> exp()
]
sch_overall <- sch_per_cnty[,
  {
    nsch_cnty <- nsch_base[ll:ul]
    nsch_cnty_share <- nsch_cnty / sum(nsch_cnty)
    .(
      cnty_OR,
      schl_OR = draw_constrained_normal(nsch_cnty_share, sigma_sch) |> exp()
    )
  },
  by = parent_id
]

# row == time, col = school
schl_prob_matrix <- outer(
  phi_st_OR,
  sch_overall[, cnty_OR * schl_OR],
  "*"
) |>
  odds_to_p()

cnty_prob_matrix <- sch_per_cnty[,
  {
    schl_prob_matrix[, ll:ul, drop = FALSE] %*%
      nsch_base[ll:ul] /
      sum(nsch_base[ll:ul])
  },
  by = parent_id
]$V1 |>
  matrix(ncol = length(county_names))

phi_st <- cnty_prob_matrix %*% ncty_share


################################################################################
# A.3 ChildVaxView #############################################################
################################################################################

# ChildVaxView occurs at the State level. For example purposes, we're treating
# the observations as if they are censored (i.e. minimum) levels of vaccination
# The observations are linked: when the cohort turns 3, we get an observation
# at 36 months (now) and of 24 months (now - 1 year, so delayed).
#
# The last ChildVaxView Sample should correspond to the last observed cohort

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
  data.table(
    loc_id = "State",
    parent_id = NA_character_,
    cohort = seq_len(n_cohort),
    age_min = 2,
    positive = at_24,
    sample_n = n_cvv
  ),
  data.table(
    loc_id = "State",
    parent_id = NA_character_,
    cohort = seq_len(n_cohort),
    age_min = 3,
    positive = at_36,
    sample_n = n_cvv
  )
)[, dose := 1L][, censored := 1.0]

# TeenVaxView occurs at the State level; we simulate an independent set of
# participants every year.

sim_teen <- data.table(
  loc_id = "State",
  parent_id = NA_character_,
  cohort = tvv_cohorts,
  positive = numeric(length(tvv_cohorts)),
  sample_n = numeric(length(tvv_cohorts)),
  age_min = min(study_ages),
  age_max = max(study_ages) + 1L
)

for (i in seq_len(nrow(sim_teen))) {
  samp_size <- as.integer(runif(length(study_ages), 40, 70))
  # constant cohort + age => ref cohort (tvv_cohorts[i]) + max(study_ages)
  phi_slice <- tvv_cohorts[i] + max(study_ages) - study_ages
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

################################################################################
# A.4 School-level data for K entry ############################################
################################################################################

kg_sim_full <- list()
cnty_ids <- with(sch_per_cnty, rep(parent_id, times = n_sch))
for (s in seq_len(tot_sch)) {
  nsch <- integer(length(skv_cohorts))
  nsch[1] <- nsch_base[s]
  for (y in 2:length(skv_cohorts)) {
    nsch[y] <- nsch[y - 1] + as.integer(round(5 * runif(1, min = -1, max = 1)))
    if (nsch[y] < 4) {
      nsch[y] <- 4L
    }
  }
  cov_temp <- schl_prob_matrix[skv_cohorts, s] * cov[sch_start, 2]
  kg_sim_full[[s]] <- data.frame(
    cohort = skv_cohorts,
    parent_id = cnty_ids[s],
    loc_id = school_names[s],
    positive = rbinom(length(skv_cohorts), nsch, cov_temp),
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
  by = cohort
][, {
  npos <- rbinom(.N, tot_vax, 0.9)
  .(
    loc_id = "State",
    parent_id = NA_character_,
    sample_n = npos + rbinom(.N, tot_non, 0.9),
    positive = npos,
    cohort,
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

# Assign sequential obs_id
observations_sim$obs_id <- seq_len(nrow(observations_sim))

# Calculate normalized cohorts (using age_min and age_max)
obs_for_pop <- copy(observations_sim)
# the youngest age
# obs_for_pop[, cohort := year - min(year) + 1L]

populations_sim <- imuGAP:::create_observation_populations(
  obs_for_pop,
  mode = "snapshot"
)

# Create locations mapping with population count per entity
school_pops <- data.table(loc_id = school_names, population = as.numeric(nsch_base))
county_pops <- data.table(loc_id = county_names, population = as.numeric(ncty_base))
state_pop <- data.table(loc_id = "State", population = as.numeric(sum(ncty_base)))
pop_dt <- rbind(state_pop, county_pops, school_pops)

locations_sim <- rbindlist(
  list(
    data.frame(loc_id = "State", parent_id = NA_character_),
    data.frame(loc_id = county_names, parent_id = "State"),
    unique(observations_sim[loc_id != "State", .(loc_id, parent_id)])
  ),
  use.names = TRUE,
  fill = TRUE
)

locations_sim <- pop_dt[setDT(locations_sim), on = "loc_id"]

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
  schl_OR = sch_overall$schl_OR,
  cnty_OR = sch_per_cnty$cnty_OR,
  censor_reduction = other_vax_reduction,
  uptake = cov,
  county_names = county_names,
  school_names = school_names,
  cnty_ids = cnty_ids
)
saveRDS(sim_internals, "data-raw/sim_internals.rds")

# --- Target population for prediction --------------------------------------
target_grid <- imuGAP:::create_target(
  location = unique(locations_sim$loc_id),
  age = 1:18,
  cohort = max(populations_sim$cohort) - 18,
  dose = c(1, 2),
  mode = "snapshot"
)
saveRDS(target_grid, file = "data-raw/target_sim.rds")

coverage <- target_grid[,
  fcase(
    loc_id == "State"                                                          ,
    sim_internals$phi_st[cohort]                                               ,
    loc_id %in% sim_internals$county_names                                     ,
    cnty_prob_matrix[cbind(cohort, match(loc_id, sim_internals$county_names))] ,
    loc_id %in% sim_internals$school_names                                     ,
    schl_prob_matrix[cbind(cohort, match(loc_id, sim_internals$school_names))]
  ) *
    sim_internals$uptake[cbind(age, dose)]
]

latent_params_sim <- list(
  phi_state = sim_internals$phi_st,
  lambda = sim_internals$lambda,
  sigma_sch = sim_internals$sigma_sch,
  sigma_cnty = sim_internals$sigma_cnty,
  schl_OR = sim_internals$schl_OR,
  cnty_OR = sim_internals$cnty_OR,
  censor_reduction = sim_internals$censor_reduction,
  uptake = sim_internals$uptake,
  coverage = coverage
)
usethis::use_data(latent_params_sim, overwrite = TRUE)
