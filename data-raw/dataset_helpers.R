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
get_simulation_setup <- function(
  seed = 93254,
  sigma_sch = 0.8,
  sigma_cnty = 0.4,
  other_vax_reduction = 0.95
) {
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

  # True state-level lifetime NON-uptake proportion (phi) across cohorts generated via B-spline.
  # Model parameterization: logit(phi_st) = bs * beta_bs, where phi represents non-uptake
  # and (1 - phi_st_target) gives the vaccinating population uptake propensity (~84% - 94%).
  df_bs <- 5L
  bsp <- splines::bs(seq_len(n_cohort), df = df_bs, intercept = TRUE)
  beta_bs <- c(-1.67, -1.82, -2.47, -2.94, -2.51)
  phi_st_target <- stats::plogis(as.vector(bsp %*% beta_bs))

  lambda <- c(2.8, 3.0)
  n_doses <- length(lambda)
  dose_schedule <- c(1, 4)
  doses <- matrix(0, ncol = length(dose_schedule), nrow = n_yr)
  for (i in seq_along(dose_schedule)) {
    doses[(dose_schedule[i] + 1):nrow(doses), i] <- 1
  }

  # Continuous-time Markov multi-state transition matrix exponential
  n_states <- n_doses + 1L
  p_state <- c(1, rep(0, n_states - 1L))
  cov <- matrix(0, nrow = n_yr, ncol = n_doses)
  for (y in 1:n_yr) {
    r1 <- doses[y, 1] * lambda[1]
    r2 <- doses[y, 2] * lambda[2]
    P <- matrix(0, 3, 3)
    P[3, 3] <- 1
    if (r1 == 0 && r2 == 0) {
      P[1, 1] <- 1
      P[2, 2] <- 1
    } else if (r1 > 0 && r2 == 0) {
      P[1, 1] <- exp(-r1)
      P[1, 2] <- 1 - exp(-r1)
      P[2, 2] <- 1
    } else if (r1 == 0 && r2 > 0) {
      P[1, 1] <- 1
      P[2, 2] <- exp(-r2)
      P[2, 3] <- 1 - exp(-r2)
    } else if (abs(r1 - r2) < 1e-12) {
      P[1, 1] <- exp(-r1)
      P[1, 2] <- r1 * exp(-r1)
      P[1, 3] <- 1 - (1 + r1) * exp(-r1)
      P[2, 2] <- exp(-r2)
      P[2, 3] <- 1 - exp(-r2)
    } else {
      P[1, 1] <- exp(-r1)
      P[1, 2] <- r1 / (r2 - r1) * (exp(-r1) - exp(-r2))
      P[1, 3] <- 1 - P[1, 1] - P[1, 2]
      P[2, 2] <- exp(-r2)
      P[2, 3] <- 1 - exp(-r2)
    }
    p_state <- as.vector(p_state %*% P)
    for (k in 1:n_doses) {
      cov[y, k] <- sum(p_state[(k + 1L):n_states])
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
    beta_bs = beta_bs,
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

#' Generate latent probability matrices under current logit offset model
generate_latent_current <- function(setup) {
  sch_per_cnty <- copy(setup$sch_per_cnty)

  # Enforce per-parent weighted balanced offsets and full-layer population scaling on county offsets
  w_cnty <- setup$ncty_base / sum(setup$ncty_base)
  w_prime_cnty <- sqrt(w_cnty)
  z_proj_cnty <- setup$z_raw_cnty -
    w_prime_cnty * sum(w_prime_cnty * setup$z_raw_cnty)
  scale_cnty <- sqrt(mean(setup$ncty_base) / setup$ncty_base)
  delta_cnty <- z_proj_cnty * scale_cnty * setup$sigma_cnty
  names(delta_cnty) <- setup$county_names

  # Enforce per-parent weighted balanced offsets and full-layer population scaling on school offsets
  z_raw_sch <- setup$z_raw_sch
  z_proj_sch <- numeric(length(z_raw_sch))
  for (c_idx in seq_along(setup$county_names)) {
    ll <- sch_per_cnty$ll[c_idx]
    ul <- sch_per_cnty$ul[c_idx]
    pop_slice <- setup$nsch_base[ll:ul]
    w_sch <- pop_slice / sum(pop_slice)
    w_prime_sch <- sqrt(w_sch)
    z_slice <- z_raw_sch[ll:ul]
    z_proj_sch[ll:ul] <- z_slice - w_prime_sch * sum(w_prime_sch * z_slice)
  }
  scale_sch <- sqrt(mean(setup$nsch_base) / setup$nsch_base)
  delta_sch <- z_proj_sch * scale_sch * setup$sigma_sch
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

  # Note: (1 - phi_st) represents the vaccinating population uptake propensity
  p_24 <- pmin(pmax((1 - phi_st) * cov[2, 1], 0), 1)
  p_36 <- pmin(pmax((1 - phi_st) * cov[3, 1], 0), 1)
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
  )[, dose := 1L][, censored := NA_real_]

  # 2. TeenVaxView using equal cohort sample sizes and pre-drawn uniform quantiles
  n_teen_per_age <- 50L
  total_teen_samp <- n_teen_per_age * length(study_ages)
  sim_teen <- data.table(
    loc_id = "State",
    parent_id = NA_character_,
    cohort = tvv_cohorts,
    positive = numeric(length(tvv_cohorts)),
    sample_n = total_teen_samp,
    age_min = min(study_ages),
    age_max = max(study_ages) + 1L
  )

  for (i in seq_len(nrow(sim_teen))) {
    u_slice <- setup$u_teen[i, ]
    phi_slice <- tvv_cohorts[i] + max(study_ages) - study_ages
    p_slice <- pmin(pmax((1 - phi_st[phi_slice]) * cov[study_ages, 2], 0), 1)

    sim_teen$positive[i] <- sum(qbinom(u_slice, n_teen_per_age, p_slice))
  }
  sim_teen$dose <- 2L

  # 3. School kindergarten entry data using pre-drawn uniform quantiles
  kg_sim_full <- list()
  for (s in seq_len(tot_sch)) {
    nsch <- nsch_matrix[skv_cohorts, s]
    u_vector <- setup$u_sch_matrix[, s]
    p_vector <- pmin(
      pmax((1 - schl_prob_matrix[skv_cohorts, s]) * cov[sch_start, 2L], 0),
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

  # 4. SchoolVaxView (State-level kindergarten entry) directly from state-level parameter
  n_skv_state <- as.integer(round(mean(colSums(nsch_matrix)) * 0.9))
  p_skv_state <- pmin(
    pmax((1 - phi_st[skv_cohorts]) * cov[sch_start, 2L], 0),
    1
  )
  sim_school <- data.table(
    loc_id = "State",
    parent_id = NA_character_,
    sample_n = n_skv_state,
    positive = qbinom(
      setup$u_sch_agg_pos[skv_cohorts],
      n_skv_state,
      p_skv_state
    ),
    cohort = skv_cohorts,
    age_min = sch_start,
    dose = 2L
  )

  # 5. County-level 6th grade survey (age 11, dose 2, censored)
  sim_county_full <- list()
  for (c in seq_along(county_names)) {
    ncnty <- setup$n_grade6_matrix[, c]
    u_vector <- setup$u_grade6_matrix[, c]
    p_vector <- pmin(
      pmax(
        (1 - cnty_prob_matrix[grade6_cohorts, c]) *
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
    (1 -
      fcase(
        loc_id == "State"                                            ,
        phi_st[cohort]                                               ,
        loc_id %in% county_names                                     ,
        cnty_prob_matrix[cbind(cohort, match(loc_id, county_names))] ,
        loc_id %in% school_names                                     ,
        schl_prob_matrix[cbind(cohort, match(loc_id, school_names))]
      )) *
      cov[cbind(age, dose)]
  ]

  latent_params_sim <- list(
    beta_bs = setup$beta_bs,
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

#' Simulate observations with minimum noise (expected values) from setup and latent objects
simulate_observations_from_latent_min_noise <- function(
  setup,
  latent,
  uncensored = TRUE
) {
  n_cohort <- setup$n_cohort
  cov <- setup$cov
  phi_st <- latent$phi_st
  cnty_prob_matrix <- latent$cnty_prob_matrix
  schl_prob_matrix <- latent$schl_prob_matrix
  other_vax_reduction <- if (uncensored) 1.0 else setup$other_vax_reduction
  study_ages <- setup$study_ages
  tvv_cohorts <- setup$tvv_cohorts
  skv_cohorts <- setup$skv_cohorts
  sch_start <- setup$sch_start
  tot_sch <- setup$tot_sch
  school_names <- setup$school_names
  county_names <- setup$county_names
  nsch_base <- setup$nsch_base
  nsch_matrix <- setup$nsch_matrix
  ncty_base <- setup$ncty_base
  grade6_start <- setup$grade6_start
  grade6_cohorts <- setup$grade6_cohorts
  cnty_ids <- setup$cnty_ids

  # 1. ChildVaxView using expected values
  n_cvv <- setup$n_cvv
  p_24 <- pmin(pmax((1 - phi_st) * cov[2, 1], 0), 1)
  p_36 <- pmin(pmax((1 - phi_st) * cov[3, 1], 0), 1)

  at_24 <- as.integer(round(n_cvv * p_24))
  at_36 <- as.integer(round(n_cvv * p_36))

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
  )[, dose := 1L][, censored := NA_real_]

  # 2. TeenVaxView using expected values and equal cohort sample sizes
  n_teen_per_age <- 50L
  total_teen_samp <- n_teen_per_age * length(study_ages)
  sim_teen <- data.table(
    loc_id = "State",
    parent_id = NA_character_,
    cohort = tvv_cohorts,
    positive = numeric(length(tvv_cohorts)),
    sample_n = total_teen_samp,
    age_min = min(study_ages),
    age_max = max(study_ages) + 1L
  )

  for (i in seq_len(nrow(sim_teen))) {
    phi_slice <- tvv_cohorts[i] + max(study_ages) - study_ages
    p_slice <- pmin(pmax((1 - phi_st[phi_slice]) * cov[study_ages, 2], 0), 1)

    sim_teen$positive[i] <- as.integer(round(sum(n_teen_per_age * p_slice)))
  }
  sim_teen$dose <- 2L

  # 3. School kindergarten entry data using expected values
  kg_sim_full <- list()
  for (s in seq_len(tot_sch)) {
    nsch <- nsch_matrix[skv_cohorts, s]
    p_vector <- pmin(
      pmax((1 - schl_prob_matrix[skv_cohorts, s]) * cov[sch_start, 2L], 0),
      1
    )

    kg_sim_full[[s]] <- data.table(
      cohort = skv_cohorts,
      parent_id = cnty_ids[s],
      loc_id = school_names[s],
      positive = as.integer(round(nsch * p_vector)),
      sample_n = nsch,
      age_min = sch_start,
      dose = 2L
    )
  }
  kg_sim <- rbindlist(kg_sim_full)

  # 4. SchoolVaxView (State-level kindergarten entry) directly from state-level parameter
  n_skv_state <- as.integer(round(mean(colSums(nsch_matrix)) * 0.9))
  p_skv_state <- pmin(
    pmax((1 - phi_st[skv_cohorts]) * cov[sch_start, 2L], 0),
    1
  )
  sim_school <- data.table(
    loc_id = "State",
    parent_id = NA_character_,
    sample_n = n_skv_state,
    positive = as.integer(round(n_skv_state * p_skv_state)),
    cohort = skv_cohorts,
    age_min = sch_start,
    dose = 2L
  )

  # 5. County-level 6th grade survey (age 11, dose 2) using expected values
  sim_county_full <- list()
  for (c in seq_along(county_names)) {
    ncnty <- setup$n_grade6_matrix[, c]
    p_vector <- pmin(
      pmax(
        (1 - cnty_prob_matrix[grade6_cohorts, c]) *
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
      positive = as.integer(round(ncnty * p_vector)),
      sample_n = ncnty,
      dose = 2L,
      censored = if (uncensored) NA_real_ else 1.0
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
    (1 -
      fcase(
        loc_id == "State"                                            ,
        phi_st[cohort]                                               ,
        loc_id %in% county_names                                     ,
        cnty_prob_matrix[cbind(cohort, match(loc_id, county_names))] ,
        loc_id %in% school_names                                     ,
        schl_prob_matrix[cbind(cohort, match(loc_id, school_names))]
      )) *
      cov[cbind(age, dose)]
  ]

  latent_params_sim <- list(
    beta_bs = setup$beta_bs,
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
