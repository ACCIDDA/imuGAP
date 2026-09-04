# Shared code and helper functions for reference dataset generation
#
# Supports simulation setup, latent parameter generation, and observation simulation.

library(data.table)

# Declare global variables used in data.table syntax
utils::globalVariables(c(
  "ul",
  "ll",
  "n_sch",
  "parent_id",
  "positive",
  "sample_n",
  "tot_vax",
  "tot_non",
  "dose",
  "censored",
  "cohort"
))

p_to_odds <- function(p) p / (1 - p)
odds_to_p <- function(odds) odds / (1 + odds)

#' Construct simulation setup containing population structure and pre-drawn random values
get_simulation_setup <- function(seed = 93254) {
  set.seed(seed)

  n_yr <- 33L
  n_cohort <- 30L

  # ChildVaxView (CVV) provides the most recent observation of the latest observed cohort,
  # setting the temporal boundary for all other observation streams. Because max age + cohort
  # must be conserved across all observations, the maximum observed cohort for an observation
  # at age A is: max_cohort = n_cohort + cvv_max_age - A.
  cvv_max_age <- 3L
  cvv_max_cohort <- n_cohort
  cvv_cohorts <- seq_len(cvv_max_cohort)

  # TeenVaxView (TVV) spanning study ages 18 down to 14
  tvv_max_age <- 18L
  tvv_min_age <- 14L
  study_ages <- tvv_max_age:tvv_min_age
  tvv_max_cohort <- n_cohort + cvv_max_age - tvv_max_age
  tvv_cohorts <- seq_len(tvv_max_cohort)

  # School-level & SchoolVaxView (SKV) kindergarten entry at age 5
  sch_start <- 5L
  skv_max_cohort <- n_cohort + cvv_max_age - sch_start
  skv_cohorts <- seq_len(skv_max_cohort)

  # County-level 6th grade entry survey at age 11
  grade6_start <- 11L
  grade6_max_cohort <- n_cohort + cvv_max_age - grade6_start
  grade6_cohorts <- seq_len(grade6_max_cohort)

  phi_st_target <- c(
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

  lambda <- c(2.8, 3.0)
  n_doses <- length(lambda)
  dose_schedule <- c(1, 4)
  doses <- matrix(0, ncol = length(dose_schedule), nrow = n_yr)
  for (i in seq_along(dose_schedule)) {
    doses[(dose_schedule[i] + 1):nrow(doses), i] <- 1
  }

  cov <- matrix(data = 0, nrow = n_yr, ncol = n_doses)
  for (d in seq_len(n_doses)) {
    ref <- if (d == 1L) rep(1, n_yr) else cov[, d - 1L]
    survival <- (1 - exp(-lambda[d] * doses[, d]))
    for (i in 2:n_yr) {
      cov[i, d] <- cov[i - 1, d] + (ref[i] - cov[i - 1, d]) * survival[i]
    }
  }

  county_names <- c("Scruggs", "Simone", "Watson")
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
  )[, ul := cumsum(n_sch)][, ll := c(0L, head(ul, -1L)) + 1L]
  tot_sch <- sum(sch_per_cnty$n_sch)
  cnty_ids <- with(sch_per_cnty, rep(parent_id, times = n_sch))

  sigma_sch <- 0.8
  sigma_cnty <- 0.4
  other_vax_reduction <- 0.95

  # Initial enrollment sampling
  nsch_start <- rlnorm(tot_sch, log(75), log(2.5))
  badindices <- which(nsch_start < 10 | nsch_start > 450)
  while (length(badindices)) {
    nsch_start[badindices] <- rlnorm(length(badindices), log(75), log(2.5))
    badindices <- which(nsch_start < 10 | nsch_start > 450)
  }
  nsch_start <- as.integer(round(nsch_start))

  # Generate dynamic school and county enrollment matrices over cohorts
  nsch_matrix <- matrix(0L, nrow = n_cohort, ncol = tot_sch)
  nsch_matrix[1, ] <- nsch_start
  for (s in seq_len(tot_sch)) {
    for (y in 2:n_cohort) {
      nsch_matrix[y, s] <- nsch_matrix[y - 1, s] +
        as.integer(round(5 * runif(1, min = -1, max = 1)))
      if (nsch_matrix[y, s] < 4L) nsch_matrix[y, s] <- 4L
    }
  }

  ncty_matrix <- matrix(0L, nrow = n_cohort, ncol = length(county_names))
  for (y in seq_len(n_cohort)) {
    ncty_matrix[y, ] <- sch_per_cnty[, mapply(
      function(l, u) sum(nsch_matrix[y, l:u]),
      ll,
      ul
    )]
  }

  # Static baseline weighting derived from mean realized enrollment across cohorts
  nsch_base <- colMeans(nsch_matrix)
  ncty_base <- colMeans(ncty_matrix)
  ncty_share <- ncty_base / sum(ncty_base)

  # Pre-draw random standard normals for counties and schools
  z_raw_cnty <- rnorm(length(county_names))
  z_raw_sch <- rnorm(tot_sch)

  # Pre-draw uniform quantiles and sample sizes
  n_cvv <- as.integer(round(runif(n_cohort, 250, 450)))
  u_cvv_24 <- runif(n_cohort)
  u_cvv_36 <- runif(n_cohort)

  teen_samp_sizes <- matrix(
    0L,
    nrow = length(tvv_cohorts),
    ncol = length(study_ages)
  )
  u_teen <- matrix(0, nrow = length(tvv_cohorts), ncol = length(study_ages))
  for (i in seq_along(tvv_cohorts)) {
    teen_samp_sizes[i, ] <- as.integer(runif(length(study_ages), 40, 70))
    u_teen[i, ] <- runif(length(study_ages))
  }

  u_sch_matrix <- matrix(0, nrow = length(skv_cohorts), ncol = tot_sch)
  for (s in seq_len(tot_sch)) {
    u_sch_matrix[, s] <- runif(length(skv_cohorts))
  }

  u_sch_agg_pos <- runif(n_cohort)
  u_sch_agg_non <- runif(n_cohort)

  n_grade6_matrix <- matrix(
    0L,
    nrow = length(grade6_cohorts),
    ncol = length(county_names)
  )
  u_grade6_matrix <- matrix(
    0,
    nrow = length(grade6_cohorts),
    ncol = length(county_names)
  )
  for (c in seq_along(county_names)) {
    n_grade6_matrix[, c] <- as.integer(round(runif(
      length(grade6_cohorts),
      120,
      250
    )))
    u_grade6_matrix[, c] <- runif(length(grade6_cohorts))
  }

  list(
    county_names = county_names,
    cov = cov,
    cvv_max_age = cvv_max_age,
    cvv_max_cohort = cvv_max_cohort,
    cvv_cohorts = cvv_cohorts,
    grade6_cohorts = grade6_cohorts,
    grade6_max_cohort = grade6_max_cohort,
    grade6_start = grade6_start,
    lambda = lambda,
    n_cohort = n_cohort,
    n_cvv = n_cvv,
    n_grade6_matrix = n_grade6_matrix,
    n_yr = n_yr,
    ncty_base = ncty_base,
    ncty_matrix = ncty_matrix,
    ncty_share = ncty_share,
    nsch_base = nsch_base,
    nsch_matrix = nsch_matrix,
    other_vax_reduction = other_vax_reduction,
    phi_st_target = phi_st_target,
    sch_per_cnty = sch_per_cnty,
    sch_start = sch_start,
    school_names = school_names,
    sigma_cnty = sigma_cnty,
    sigma_sch = sigma_sch,
    skv_cohorts = skv_cohorts,
    skv_max_cohort = skv_max_cohort,
    study_ages = study_ages,
    teen_samp_sizes = teen_samp_sizes,
    tot_sch = tot_sch,
    tvv_cohorts = tvv_cohorts,
    tvv_max_cohort = tvv_max_cohort,
    u_cvv_24 = u_cvv_24,
    u_cvv_36 = u_cvv_36,
    u_grade6_matrix = u_grade6_matrix,
    u_sch_agg_non = u_sch_agg_non,
    u_sch_agg_pos = u_sch_agg_pos,
    u_sch_matrix = u_sch_matrix,
    u_teen = u_teen,
    z_raw_cnty = z_raw_cnty,
    z_raw_sch = z_raw_sch,
    cnty_ids = cnty_ids
  )
}

#' Compute orthonormal basis Q* for weighted sum-to-zero constraint
get_weighted_qr_basis <- function(w) {
  w <- as.numeric(w)
  w <- w / sum(w)
  K <- length(w)
  v1 <- w / sqrt(sum(w^2))
  qr_decomp <- qr(cbind(v1, diag(K)))
  Q <- qr.Q(qr_decomp)
  Q[, 2:K, drop = FALSE]
}

#' Generate latent probability matrices under weighted balanced logit offset model
generate_latent_current <- function(setup) {
  sch_per_cnty <- copy(setup$sch_per_cnty)

  # County offsets: balanced with county population shares
  w_cnty <- setup$ncty_base / sum(setup$ncty_base)
  Q_star_cnty <- get_weighted_qr_basis(w_cnty)
  z_cnty <- setup$z_raw_cnty[seq_len(length(setup$county_names) - 1L)]
  delta_cnty <- as.vector(Q_star_cnty %*% z_cnty) * setup$sigma_cnty
  names(delta_cnty) <- setup$county_names

  # School offsets: balanced per county with school population shares
  delta_sch <- numeric(setup$tot_sch)
  for (c_idx in seq_along(setup$county_names)) {
    ll <- sch_per_cnty$ll[c_idx]
    ul <- sch_per_cnty$ul[c_idx]
    k_sch <- ul - ll + 1L
    w_sch <- setup$nsch_base[ll:ul] / sum(setup$nsch_base[ll:ul])
    Q_star_sch <- get_weighted_qr_basis(w_sch)
    z_sch_c <- setup$z_raw_sch[ll:(ll + k_sch - 2L)]
    delta_sch[ll:ul] <- as.vector(Q_star_sch %*% z_sch_c) * setup$sigma_sch
  }
  names(delta_sch) <- setup$school_names

  state_logit <- qlogis(setup$phi_st_target)

  # Expand school logit matrix: n_cohort x tot_sch
  schl_prob_matrix <- matrix(0, nrow = setup$n_cohort, ncol = setup$tot_sch)
  for (c_idx in seq_along(setup$county_names)) {
    ll <- sch_per_cnty$ll[c_idx]
    ul <- sch_per_cnty$ul[c_idx]
    for (s in ll:ul) {
      schl_logit <- state_logit + delta_cnty[c_idx] + delta_sch[s]
      schl_prob_matrix[, s] <- plogis(schl_logit)
    }
  }

  # County prob matrix
  cnty_prob_matrix <- matrix(
    0,
    nrow = setup$n_cohort,
    ncol = length(setup$county_names)
  )
  for (c_idx in seq_along(setup$county_names)) {
    cnty_prob_matrix[, c_idx] <- plogis(state_logit + delta_cnty[c_idx])
  }

  list(
    approach = "current",
    approach_name = "Current Logit Offset Model",
    phi_st = setup$phi_st_target,
    cnty_prob_matrix = cnty_prob_matrix,
    schl_prob_matrix = schl_prob_matrix,
    delta_cnty = delta_cnty,
    delta_sch = delta_sch
  )
}

#' Simulate observations and construct package fixtures from setup and latent objects
simulate_observations_from_latent <- function(setup, latent, obs_seed = 93254) {
  set.seed(obs_seed)

  n_cohort <- setup$n_cohort
  cov <- setup$cov
  phi_st <- latent$phi_st
  cnty_prob_matrix <- latent$cnty_prob_matrix
  schl_prob_matrix <- latent$schl_prob_matrix
  other_vax_reduction <- setup$other_vax_reduction
  study_ages <- setup$study_ages
  tvv_cohorts <- setup$tvv_cohorts
  skv_cohorts <- setup$skv_cohorts
  sch_start <- setup$sch_start
  tot_sch <- setup$tot_sch
  sch_per_cnty <- setup$sch_per_cnty
  school_names <- setup$school_names
  county_names <- setup$county_names
  nsch_base <- setup$nsch_base
  nsch_matrix <- setup$nsch_matrix
  ncty_base <- setup$ncty_base
  grade6_start <- setup$grade6_start
  grade6_cohorts <- setup$grade6_cohorts
  cnty_ids <- setup$cnty_ids

  # 1. ChildVaxView using pre-drawn uniform quantiles
  n_cvv <- setup$n_cvv
  u_cvv_24 <- setup$u_cvv_24
  u_cvv_36 <- setup$u_cvv_36

  p_24 <- pmin(pmax(phi_st * cov[2, 1] * other_vax_reduction, 0), 1)
  p_36 <- pmin(pmax(phi_st * cov[3, 1] * other_vax_reduction, 0), 1)
  p_36_cond <- pmin(pmax((p_36 - p_24) / (1 - p_24), 0), 1)

  at_24 <- qbinom(u_cvv_24, n_cvv, p_24)
  rem_n <- pmax(n_cvv - at_24, 0L)
  at_36 <- at_24 + qbinom(u_cvv_36, rem_n, p_36_cond)

  sim_child <- rbind(
    data.table(
      loc_id = "State",
      parent_id = NA_character_,
      cohort = seq_len(n_cohort),
      age_min = 2L,
      positive = at_24,
      sample_n = n_cvv
    ),
    data.table(
      loc_id = "State",
      parent_id = NA_character_,
      cohort = seq_len(n_cohort),
      age_min = 3L,
      positive = at_36,
      sample_n = n_cvv
    )
  )[, dose := 1L][, censored := 1.0]

  # 2. TeenVaxView using pre-drawn uniform quantiles
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
    samp_size <- setup$teen_samp_sizes[i, ]
    u_slice <- setup$u_teen[i, ]
    phi_slice <- tvv_cohorts[i] + max(study_ages) - study_ages
    p_slice <- pmin(pmax(phi_st[phi_slice] * cov[study_ages, 2], 0), 1)

    sim_teen$sample_n[i] <- sum(samp_size)
    sim_teen$positive[i] <- sum(qbinom(u_slice, samp_size, p_slice))
  }
  sim_teen$dose <- 2L

  # 3. School kindergarten entry data using pre-drawn uniform quantiles
  kg_sim_full <- list()
  for (s in seq_len(tot_sch)) {
    nsch <- nsch_matrix[skv_cohorts, s]
    u_vector <- setup$u_sch_matrix[, s]
    p_vector <- pmin(
      pmax(schl_prob_matrix[skv_cohorts, s] * cov[sch_start, 2L], 0),
      1
    )

    kg_sim_full[[s]] <- data.table(
      cohort = skv_cohorts,
      parent_id = cnty_ids[s],
      loc_id = school_names[s],
      positive = qbinom(u_vector, nsch, p_vector),
      sample_n = nsch,
      age_min = sch_start,
      dose = 2L
    )
  }
  kg_sim <- rbindlist(kg_sim_full)

  # 4. Aggregate school data into SchoolVaxView (State-level kindergarten entry)
  sim_school <- kg_sim[
    age_min == sch_start & dose == 2L,
    {
      tot_vax <- sum(positive)
      .(tot_vax = tot_vax, tot_non = sum(sample_n) - tot_vax)
    },
    by = cohort
  ][, {
    npos <- qbinom(setup$u_sch_agg_pos[skv_cohorts], tot_vax, 0.9)
    tot_non_pos <- qbinom(
      setup$u_sch_agg_non[skv_cohorts],
      pmax(tot_non, 0L),
      0.9
    )
    .(
      loc_id = "State",
      parent_id = NA_character_,
      sample_n = npos + tot_non_pos,
      positive = npos,
      cohort = cohort,
      age_min = sch_start,
      dose = 2L
    )
  }]

  # 5. County-level 6th grade survey (age 11, dose 2, censored)
  sim_county_full <- list()
  for (c in seq_along(county_names)) {
    ncnty <- setup$n_grade6_matrix[, c]
    u_vector <- setup$u_grade6_matrix[, c]
    p_vector <- pmin(
      pmax(
        cnty_prob_matrix[grade6_cohorts, c] *
          cov[grade6_start, 2L] *
          other_vax_reduction,
        0
      ),
      1
    )

    sim_county_full[[c]] <- data.table(
      loc_id = county_names[c],
      parent_id = "State",
      cohort = grade6_cohorts,
      age_min = grade6_start,
      positive = qbinom(u_vector, ncnty, p_vector),
      sample_n = ncnty,
      dose = 2L,
      censored = 1.0
    )
  }
  sim_county <- rbindlist(sim_county_full)

  vv_sim <- rbindlist(
    list(sim_child, sim_school, sim_teen, sim_county),
    use.names = TRUE,
    fill = TRUE
  )

  observations_sim <- rbindlist(
    list(kg_sim, vv_sim),
    use.names = TRUE,
    fill = TRUE
  )
  observations_sim$obs_id <- seq_len(nrow(observations_sim))
  observations_sim$cohort_min <- observations_sim$cohort

  obs_for_pop <- copy(observations_sim)
  populations_sim <- imuGAP:::create_observation_populations(
    obs_for_pop,
    mode = "snapshot"
  )

  school_pops <- data.table(
    loc_id = school_names,
    population = as.numeric(nsch_base)
  )
  county_pops <- data.table(
    loc_id = county_names,
    population = as.numeric(ncty_base)
  )
  state_pop <- data.table(
    loc_id = "State",
    population = as.numeric(sum(ncty_base))
  )
  pop_dt <- rbind(state_pop, county_pops, school_pops)

  locs_raw <- unique(rbindlist(
    list(
      data.table(loc_id = "State", parent_id = NA_character_),
      data.table(loc_id = county_names, parent_id = "State"),
      data.table(loc_id = school_names, parent_id = cnty_ids)
    ),
    use.names = TRUE,
    fill = TRUE
  ))
  locations_sim <- pop_dt[locs_raw, on = "loc_id"]

  sim_internals <- list(
    phi_st = phi_st,
    lambda = setup$lambda,
    sigma_sch = setup$sigma_sch,
    sigma_cnty = setup$sigma_cnty,
    off_sch = latent$delta_sch,
    off_cnty = latent$delta_cnty,
    censor_reduction = other_vax_reduction,
    uptake = cov,
    county_names = county_names,
    school_names = school_names,
    cnty_ids = cnty_ids
  )

  target_grid <- imuGAP:::create_target(
    location = unique(locations_sim$loc_id),
    age = 1:18,
    cohort = max(populations_sim$cohort) - 18,
    dose = c(1, 2),
    mode = "snapshot"
  )

  coverage <- target_grid[,
    fcase(
      loc_id == "State"                                            ,
      phi_st[cohort]                                               ,
      loc_id %in% county_names                                     ,
      cnty_prob_matrix[cbind(cohort, match(loc_id, county_names))] ,
      loc_id %in% school_names                                     ,
      schl_prob_matrix[cbind(cohort, match(loc_id, school_names))]
    ) *
      cov[cbind(age, dose)]
  ]

  latent_params_sim <- list(
    phi_state = phi_st,
    lambda = setup$lambda,
    sigma_sch = setup$sigma_sch,
    sigma_cnty = setup$sigma_cnty,
    off_sch = latent$delta_sch,
    off_cnty = latent$delta_cnty,
    censor_reduction = other_vax_reduction,
    uptake = cov,
    coverage = coverage
  )

  list(
    latent = latent,
    observations_sim = observations_sim,
    populations_sim = populations_sim,
    locations_sim = locations_sim,
    latent_params_sim = latent_params_sim,
    sim_internals = sim_internals,
    target_sim = target_grid
  )
}
