# Shared code and helper functions for reference dataset generation alternatives
#
# Supports 4 reference dataset generation approaches:
# 1. Odds-Ratio (OR) Balanced (Current approach)
# 2. Odds-Ratio (OR) Unbalanced
# 3. Logit-Based Offset Unbalanced
# 4. Logit-Based Offset Balanced + Moving Offset Parameter

library(data.table)

# Declare global variables used in data.table syntax
utils::globalVariables(c(
  "ul",
  "ll",
  "n_sch",
  "parent_id",
  "cnty_OR",
  "schl_OR",
  "positive",
  "sample_n",
  "tot_vax",
  "tot_non",
  "dose",
  "censored",
  "cohort",
  "obs_rate",
  "true_p",
  "Weighting"
))

p_to_odds <- function(p) p / (1 - p)
odds_to_p <- function(odds) odds / (1 + odds)

#' Project a vector z onto the space orthogonal to weights w (zero-weighted sum)
project_constrained <- function(z, w) {
  w_norm <- w / sum(w)
  mu_w <- sum(w_norm * z)
  z - mu_w
}

#' Solve for moving offset alpha(t) in logit space such that weighted average matches target_p
#'
#' sum(w_i * plogis(qlogis(target_p) + alpha + delta_i)) = target_p
solve_moving_offset <- function(target_p, weights, deltas, tol = 1e-9) {
  weights_norm <- weights / sum(weights)
  target_logit <- qlogis(target_p)

  f <- function(alpha) {
    sum(weights_norm * plogis(target_logit + alpha + deltas)) - target_p
  }

  # f(alpha) is strictly increasing in alpha
  # Find an interval where f changes sign
  lower <- -5.0
  upper <- 5.0
  while (f(lower) > 0) {
    lower <- lower * 2
  }
  while (f(upper) < 0) {
    upper <- upper * 2
  }

  uniroot(f, interval = c(lower, upper), tol = tol)$root
}

#' Solve for moving offset alpha(t) using 3rd-order Taylor expansion cubic relationship
#'
#' Derives alpha(t) by solving the polynomial equation:
#'   A * alpha^3 + B * alpha^2 + C * alpha + D = 0
#' where coefficients A, B, C, D depend on parent probability target_p and weighted moments M2, M3 of deltas:
#'   M2 = sum(w_i * delta_i^2)
#'   M3 = sum(w_i * delta_i^3)
#'   S2 = 0.5 - target_p
#'   S3 = 1/6 - target_p + target_p^2
#'   A  = S3
#'   B  = S2
#'   C  = 1 + 3 * S3 * M2
#'   D  = S2 * M2 + S3 * M3
solve_moving_offset_taylor <- function(
  target_p,
  weights,
  deltas,
  tol = 1e-9,
  max_iter = 10L
) {
  weights_norm <- weights / sum(weights)

  # Pre-compute 2nd and 3rd weighted moments of deltas
  M2 <- sum(weights_norm * (deltas^2))
  M3 <- sum(weights_norm * (deltas^3))

  S2 <- 0.5 - target_p
  S3 <- (1.0 / 6.0) - target_p + (target_p^2)

  A <- S3
  B <- S2
  C <- 1.0 + 3.0 * S3 * M2
  D <- S2 * M2 + S3 * M3

  # Initial estimate from 1st-order linear approximation: alpha_0 = -D / C
  alpha <- ifelse(abs(C) > 1e-12, -D / C, 0.0)

  # Vectorized Newton-Raphson loop to find roots of A*alpha^3 + B*alpha^2 + C*alpha + D = 0
  for (iter in seq_len(max_iter)) {
    f <- A * (alpha^3) + B * (alpha^2) + C * alpha + D
    f_prime <- 3.0 * A * (alpha^2) + 2.0 * B * alpha + C
    step <- f / f_prime
    alpha <- alpha - step
    if (max(abs(step), na.rm = TRUE) < tol) break
  }

  alpha
}

#' Explicitly calculate county and state aggregate probabilities from school probability matrix
aggregate_probabilities <- function(
  schl_prob_matrix,
  setup,
  weighting = c("static", "dynamic")
) {
  weighting <- match.arg(weighting)
  n_cohort <- setup$n_cohort
  n_cnty <- length(setup$county_names)
  tot_sch <- setup$tot_sch
  sch_per_cnty <- setup$sch_per_cnty

  cnty_prob_matrix <- matrix(0, nrow = n_cohort, ncol = n_cnty)

  if (weighting == "static") {
    nsch_base <- setup$nsch_base
    ncty_share <- setup$ncty_share

    for (c_idx in seq_len(n_cnty)) {
      ll <- sch_per_cnty$ll[c_idx]
      ul <- sch_per_cnty$ul[c_idx]
      weights_sch <- nsch_base[ll:ul] / sum(nsch_base[ll:ul])
      cnty_prob_matrix[, c_idx] <- schl_prob_matrix[, ll:ul, drop = FALSE] %*%
        weights_sch
    }
    phi_st <- as.numeric(cnty_prob_matrix %*% ncty_share)
  } else {
    nsch_matrix <- setup$nsch_matrix
    ncty_matrix <- setup$ncty_matrix

    for (t in seq_len(n_cohort)) {
      for (c_idx in seq_len(n_cnty)) {
        ll <- sch_per_cnty$ll[c_idx]
        ul <- sch_per_cnty$ul[c_idx]
        weights_sch_t <- nsch_matrix[t, ll:ul] / ncty_matrix[t, c_idx]
        cnty_prob_matrix[t, c_idx] <- sum(
          schl_prob_matrix[t, ll:ul] * weights_sch_t
        )
      }
    }
    ncty_share_dynamic <- setup$ncty_share_dynamic
    phi_st <- rowSums(cnty_prob_matrix * ncty_share_dynamic)
  }

  list(
    phi_st = phi_st,
    cnty_prob_matrix = cnty_prob_matrix
  )
}

#' Setup common setup constants and fixed base draws
get_simulation_setup <- function(seed = 93254) {
  set.seed(seed)

  n_yr <- 33L
  n_cohort <- 30L
  cvv_max_age <- 3L
  tvv_max_age <- 18L
  tvv_min_age <- 14L
  tvv_max_cohort <- n_cohort + cvv_max_age - tvv_max_age
  tvv_cohorts <- seq_len(tvv_max_cohort)
  study_ages <- tvv_max_age:tvv_min_age
  sch_start <- 5L
  skv_max_cohort <- n_cohort + cvv_max_age - sch_start
  skv_cohorts <- seq_len(skv_max_cohort)

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

  phi_st_OR <- phi_st_target |> p_to_odds()

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

  sigma_sch <- 0.8
  sigma_cnty <- 0.4
  other_vax_reduction <- 0.95

  # Initial enrollment sampling (cohort 1 seed)
  nsch_start <- rlnorm(tot_sch, log(75), log(2.5))
  badindices <- which(!between(nsch_start, 10, 450))
  while (length(badindices)) {
    nsch_start[badindices] <- rlnorm(length(badindices), log(75), log(2.5))
    badindices <- which(!between(nsch_start, 10, 450))
  }
  nsch_start <- as.integer(round(nsch_start))

  # Generate dynamic school and county enrollment matrices over cohorts (n_cohort x tot_sch)
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

  ncty_share_dynamic <- ncty_matrix / rowSums(ncty_matrix)
  nsch_share_dynamic <- matrix(0, nrow = n_cohort, ncol = tot_sch)
  for (c_idx in seq_along(county_names)) {
    ll <- sch_per_cnty$ll[c_idx]
    ul <- sch_per_cnty$ul[c_idx]
    nsch_share_dynamic[, ll:ul] <- nsch_matrix[, ll:ul] / ncty_matrix[, c_idx]
  }

  # Sample standard normal z-scores ONCE for counties and schools
  # (Ensures directional similarity across all approaches)
  z_raw_cnty <- rnorm(length(county_names))
  z_raw_sch <- rnorm(tot_sch)

  # Pre-draw uniform quantiles and fixed sample sizes ONCE across all models
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

  sch_obs_specs <- list(
    list(age_min = 2L, dose = 1L, cohorts = seq_len(n_cohort)),
    list(age_min = 3L, dose = 1L, cohorts = seq_len(n_cohort)),
    list(age_min = 5L, dose = 2L, cohorts = skv_cohorts),
    list(age_min = 14L, dose = 2L, cohorts = tvv_cohorts)
  )

  u_sch_list <- list()
  for (s in seq_len(tot_sch)) {
    for (spec_i in seq_along(sch_obs_specs)) {
      cohs <- sch_obs_specs[[spec_i]]$cohorts
      n_c <- length(cohs)
      u_sch_list[[length(u_sch_list) + 1]] <- data.table(
        school_idx = rep(s, n_c),
        spec_idx = rep(spec_i, n_c),
        cohort = cohs,
        u = runif(n_c)
      )
    }
  }
  u_sch_dt <- rbindlist(u_sch_list)

  u_sch_agg_pos <- runif(n_cohort)
  u_sch_agg_non <- runif(n_cohort)

  list(
    county_names = county_names,
    cov = cov,
    cvv_max_age = cvv_max_age,
    lambda = lambda,
    n_cohort = n_cohort,
    n_cvv = n_cvv,
    n_yr = n_yr,
    ncty_base = ncty_base,
    ncty_matrix = ncty_matrix,
    ncty_share = ncty_share,
    ncty_share_dynamic = ncty_share_dynamic,
    nsch_base = nsch_base,
    nsch_matrix = nsch_matrix,
    nsch_share_dynamic = nsch_share_dynamic,
    other_vax_reduction = other_vax_reduction,
    phi_st_OR = phi_st_OR,
    phi_st_target = phi_st_target,
    sch_obs_specs = sch_obs_specs,
    sch_per_cnty = sch_per_cnty,
    sch_start = sch_start,
    school_names = school_names,
    sigma_cnty = sigma_cnty,
    sigma_sch = sigma_sch,
    skv_cohorts = skv_cohorts,
    study_ages = study_ages,
    teen_samp_sizes = teen_samp_sizes,
    tot_sch = tot_sch,
    tvv_cohorts = tvv_cohorts,
    tvv_max_age = tvv_max_age,
    tvv_min_age = tvv_min_age,
    u_cvv_24 = u_cvv_24,
    u_cvv_36 = u_cvv_36,
    u_sch_agg_non = u_sch_agg_non,
    u_sch_agg_pos = u_sch_agg_pos,
    u_sch_dt = u_sch_dt,
    u_teen = u_teen,
    z_raw_cnty = z_raw_cnty,
    z_raw_sch = z_raw_sch
  )
}

#' Approach 1: Odds-Ratio (OR) Balanced
generate_latent_or_balanced <- function(setup) {
  sch_per_cnty <- copy(setup$sch_per_cnty)
  ncty_share <- setup$ncty_share
  nsch_base <- setup$nsch_base

  # County offsets: balanced log-odds
  delta_cnty <- project_constrained(setup$z_raw_cnty, ncty_share) *
    setup$sigma_cnty
  sch_per_cnty[, cnty_OR := exp(delta_cnty)]

  # School offsets: balanced log-odds within county
  sch_overall <- sch_per_cnty[,
    {
      nsch_cnty <- nsch_base[ll:ul]
      z_cnty_sch <- setup$z_raw_sch[ll:ul]
      delta_sch <- project_constrained(z_cnty_sch, nsch_cnty) * setup$sigma_sch
      .(
        cnty_OR = cnty_OR,
        schl_OR = exp(delta_sch),
        delta_sch = delta_sch
      )
    },
    by = parent_id
  ]

  schl_prob_matrix <- outer(
    setup$phi_st_OR,
    sch_overall[, cnty_OR * schl_OR],
    "*"
  ) |>
    odds_to_p()

  # Explicit aggregation under static baseline weights
  static_agg <- aggregate_probabilities(
    schl_prob_matrix,
    setup,
    weighting = "static"
  )
  # Explicit aggregation under dynamic enrollment shift weights
  dynamic_agg <- aggregate_probabilities(
    schl_prob_matrix,
    setup,
    weighting = "dynamic"
  )

  list(
    approach = "or_balanced",
    approach_name = "Odds-Ratio Balanced",
    phi_st = static_agg$phi_st,
    cnty_prob_matrix = static_agg$cnty_prob_matrix,
    phi_st_dynamic = dynamic_agg$phi_st,
    cnty_prob_matrix_dynamic = dynamic_agg$cnty_prob_matrix,
    schl_prob_matrix = schl_prob_matrix,
    cnty_OR = sch_per_cnty$cnty_OR,
    schl_OR = sch_overall$schl_OR,
    delta_cnty = delta_cnty,
    delta_sch = sch_overall$delta_sch
  )
}

#' Approach 2: Odds-Ratio (OR) Unbalanced
generate_latent_or_unbalanced <- function(setup) {
  sch_per_cnty <- copy(setup$sch_per_cnty)
  ncty_share <- setup$ncty_share
  nsch_base <- setup$nsch_base

  # County offsets: unconstrained raw log-odds
  delta_cnty <- setup$z_raw_cnty * setup$sigma_cnty
  sch_per_cnty[, cnty_OR := exp(delta_cnty)]

  # School offsets: unconstrained raw log-odds
  sch_overall <- sch_per_cnty[,
    {
      z_cnty_sch <- setup$z_raw_sch[ll:ul]
      delta_sch <- z_cnty_sch * setup$sigma_sch
      .(
        cnty_OR = cnty_OR,
        schl_OR = exp(delta_sch),
        delta_sch = delta_sch
      )
    },
    by = parent_id
  ]

  schl_prob_matrix <- outer(
    setup$phi_st_OR,
    sch_overall[, cnty_OR * schl_OR],
    "*"
  ) |>
    odds_to_p()

  # Explicit aggregation under static baseline weights
  static_agg <- aggregate_probabilities(
    schl_prob_matrix,
    setup,
    weighting = "static"
  )
  # Explicit aggregation under dynamic enrollment shift weights
  dynamic_agg <- aggregate_probabilities(
    schl_prob_matrix,
    setup,
    weighting = "dynamic"
  )

  list(
    approach = "or_unbalanced",
    approach_name = "Odds-Ratio Unbalanced",
    phi_st = static_agg$phi_st,
    cnty_prob_matrix = static_agg$cnty_prob_matrix,
    phi_st_dynamic = dynamic_agg$phi_st,
    cnty_prob_matrix_dynamic = dynamic_agg$cnty_prob_matrix,
    schl_prob_matrix = schl_prob_matrix,
    cnty_OR = sch_per_cnty$cnty_OR,
    schl_OR = sch_overall$schl_OR,
    delta_cnty = delta_cnty,
    delta_sch = sch_overall$delta_sch
  )
}

#' Approach 3: Logit-Based Offset Unbalanced
generate_latent_logit_unbalanced <- function(setup) {
  sch_per_cnty <- copy(setup$sch_per_cnty)
  delta_cnty <- setup$z_raw_cnty * setup$sigma_cnty
  delta_sch <- setup$z_raw_sch * setup$sigma_sch

  # State base logit
  state_logit <- qlogis(setup$phi_st_target)

  # Expand school logit matrix: T x tot_sch
  schl_prob_matrix <- matrix(0, nrow = setup$n_cohort, ncol = setup$tot_sch)

  for (c_idx in seq_along(setup$county_names)) {
    ll <- sch_per_cnty$ll[c_idx]
    ul <- sch_per_cnty$ul[c_idx]
    for (s in ll:ul) {
      schl_logit <- state_logit + delta_cnty[c_idx] + delta_sch[s]
      schl_prob_matrix[, s] <- plogis(schl_logit)
    }
  }

  # Explicit aggregation under static baseline weights
  static_agg <- aggregate_probabilities(
    schl_prob_matrix,
    setup,
    weighting = "static"
  )
  # Explicit aggregation under dynamic enrollment shift weights
  dynamic_agg <- aggregate_probabilities(
    schl_prob_matrix,
    setup,
    weighting = "dynamic"
  )

  list(
    approach = "logit_unbalanced",
    approach_name = "Logit Offset Unbalanced",
    phi_st = static_agg$phi_st,
    cnty_prob_matrix = static_agg$cnty_prob_matrix,
    phi_st_dynamic = dynamic_agg$phi_st,
    cnty_prob_matrix_dynamic = dynamic_agg$cnty_prob_matrix,
    schl_prob_matrix = schl_prob_matrix,
    cnty_OR = exp(delta_cnty),
    schl_OR = exp(delta_sch),
    delta_cnty = delta_cnty,
    delta_sch = delta_sch
  )
}

#' Approach 4: Logit-Based Offset Balanced + Moving Offset Parameter
generate_latent_logit_balanced_offset <- function(
  setup,
  method = c("taylor", "exact")
) {
  method <- match.arg(method)
  solver <- if (method == "taylor") {
    solve_moving_offset_taylor
  } else {
    solve_moving_offset
  }

  sch_per_cnty <- copy(setup$sch_per_cnty)
  ncty_share <- setup$ncty_share
  nsch_base <- setup$nsch_base

  # Balanced offsets
  delta_cnty <- project_constrained(setup$z_raw_cnty, ncty_share) *
    setup$sigma_cnty

  delta_sch <- numeric(setup$tot_sch)
  for (c_idx in seq_along(setup$county_names)) {
    ll <- sch_per_cnty$ll[c_idx]
    ul <- sch_per_cnty$ul[c_idx]
    nsch_c <- nsch_base[ll:ul]
    delta_sch[ll:ul] <- project_constrained(setup$z_raw_sch[ll:ul], nsch_c) *
      setup$sigma_sch
  }

  # Step 1: Solve state -> county moving offset alpha_cnty(t)
  cnty_prob_target <- matrix(
    0,
    nrow = setup$n_cohort,
    ncol = length(setup$county_names)
  )
  alpha_cnty <- solver(setup$phi_st_target, ncty_share, delta_cnty)

  if (length(alpha_cnty) == 1) {
    alpha_cnty <- rep(alpha_cnty, setup$n_cohort)
  }

  for (t in seq_len(setup$n_cohort)) {
    p_st_target_t <- setup$phi_st_target[t]
    a_t <- if (method == "taylor") {
      alpha_cnty[t]
    } else {
      solver(p_st_target_t, ncty_share, delta_cnty)
    }
    alpha_cnty[t] <- a_t
    cnty_prob_target[t, ] <- plogis(qlogis(p_st_target_t) + a_t + delta_cnty)
  }

  # Step 2: Solve county -> school moving offset alpha_sch,c(t)
  schl_prob_matrix <- matrix(0, nrow = setup$n_cohort, ncol = setup$tot_sch)
  alpha_sch_mat <- matrix(
    0,
    nrow = setup$n_cohort,
    ncol = length(setup$county_names)
  )

  for (c_idx in seq_along(setup$county_names)) {
    ll <- sch_per_cnty$ll[c_idx]
    ul <- sch_per_cnty$ul[c_idx]
    nsch_c <- nsch_base[ll:ul]
    delta_sch_c <- delta_sch[ll:ul]

    p_targets <- cnty_prob_target[, c_idx]
    a_sch_vec <- if (method == "taylor") {
      solver(p_targets, nsch_c, delta_sch_c)
    } else {
      sapply(p_targets, function(pt) solver(pt, nsch_c, delta_sch_c))
    }
    alpha_sch_mat[, c_idx] <- a_sch_vec

    for (t in seq_len(setup$n_cohort)) {
      p_cnty_target_t <- cnty_prob_target[t, c_idx]
      a_t <- a_sch_vec[t]
      schl_prob_matrix[t, ll:ul] <- plogis(
        qlogis(p_cnty_target_t) + a_t + delta_sch_c
      )
    }
  }

  # EXPLICIT AGGREGATION without assuming perfect mathematical adjustments
  static_agg <- aggregate_probabilities(
    schl_prob_matrix,
    setup,
    weighting = "static"
  )
  dynamic_agg <- aggregate_probabilities(
    schl_prob_matrix,
    setup,
    weighting = "dynamic"
  )

  list(
    approach = "logit_balanced_offset",
    approach_name = sprintf(
      "Logit Offset Balanced (Moving Offset, %s)",
      method
    ),
    phi_st = static_agg$phi_st,
    cnty_prob_matrix = static_agg$cnty_prob_matrix,
    phi_st_dynamic = dynamic_agg$phi_st,
    cnty_prob_matrix_dynamic = dynamic_agg$cnty_prob_matrix,
    schl_prob_matrix = schl_prob_matrix,
    cnty_OR = exp(delta_cnty),
    schl_OR = exp(delta_sch),
    delta_cnty = delta_cnty,
    delta_sch = delta_sch,
    alpha_cnty = alpha_cnty,
    alpha_sch_mat = alpha_sch_mat,
    method = method
  )
}

#' Simulate observation datasets (ChildVaxView, TeenVaxView, SchoolVaxView) from latent matrices
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

  # ChildVaxView using pre-drawn uniform quantiles
  n_cvv <- setup$n_cvv
  u_cvv_24 <- setup$u_cvv_24
  u_cvv_36 <- setup$u_cvv_36

  vax_inc <- cov[3, 1] - cov[2, 1]
  p_24 <- pmin(pmax(phi_st * cov[2, 1] * other_vax_reduction, 0), 1)
  at_24 <- qbinom(u_cvv_24, n_cvv, p_24)

  p_36_cond <- pmin(pmax(phi_st * vax_inc * other_vax_reduction, 0), 1)
  rem_n <- pmax(n_cvv - at_24, 0L)
  at_36 <- at_24 + qbinom(u_cvv_36, rem_n, p_36_cond)

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

  # TeenVaxView using pre-drawn uniform quantiles
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

  # School level data using pre-drawn uniform quantiles
  kg_sim_full <- list()
  cnty_ids <- with(sch_per_cnty, rep(parent_id, times = n_sch))
  sch_obs_specs <- setup$sch_obs_specs

  for (s in seq_len(tot_sch)) {
    for (spec_i in seq_along(sch_obs_specs)) {
      spec <- sch_obs_specs[[spec_i]]
      a_min <- spec$age_min
      d_val <- spec$dose
      cohs <- spec$cohorts
      nsch <- nsch_matrix[cohs, s]

      u_vector <- setup$u_sch_dt[school_idx == s & spec_idx == spec_i, u]
      p_vector <- pmin(
        pmax(schl_prob_matrix[cohs, s] * cov[a_min, d_val], 0),
        1
      )

      kg_sim_full[[length(kg_sim_full) + 1]] <- data.table(
        cohort = cohs,
        parent_id = cnty_ids[s],
        loc_id = school_names[s],
        positive = qbinom(u_vector, nsch, p_vector),
        sample_n = nsch,
        age_min = a_min,
        dose = d_val
      )
    }
  }
  kg_sim <- rbindlist(kg_sim_full)

  # Aggregate school data into SchoolVaxView using pre-drawn uniform quantiles
  sim_school <- kg_sim[
    age_min == sch_start & dose == 2L,
    {
      tot_vax <- sum(positive)
      .(tot_vax = tot_vax, tot_non = sum(sample_n) - tot_vax)
    },
    by = cohort
  ][, {
    npos <- qbinom(setup$u_sch_agg_pos, tot_vax, 0.9)
    tot_non_pos <- qbinom(setup$u_sch_agg_non, pmax(tot_non, 0L), 0.9)
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

  vv_sim <- rbindlist(
    list(sim_child, sim_school, sim_teen),
    use.names = TRUE,
    fill = TRUE
  )

  observations_sim <- rbindlist(
    list(kg_sim, vv_sim),
    use.names = TRUE,
    fill = TRUE
  )
  observations_sim$obs_id <- seq_len(nrow(observations_sim))

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

  sim_internals <- list(
    phi_st = phi_st,
    lambda = setup$lambda,
    sigma_sch = setup$sigma_sch,
    sigma_cnty = setup$sigma_cnty,
    schl_OR = latent$schl_OR,
    cnty_OR = latent$cnty_OR,
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
    schl_OR = latent$schl_OR,
    cnty_OR = latent$cnty_OR,
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

#' Run full reference data generation for all 4 approaches
generate_all_reference_datasets <- function(seed = 93254) {
  setup <- get_simulation_setup(seed = seed)

  latents <- list(
    or_balanced = generate_latent_or_balanced(setup),
    or_unbalanced = generate_latent_or_unbalanced(setup),
    logit_unbalanced = generate_latent_logit_unbalanced(setup),
    logit_balanced_offset = generate_latent_logit_balanced_offset(setup)
  )

  datasets <- list()
  for (name in names(latents)) {
    datasets[[name]] <- simulate_observations_from_latent(
      setup,
      latents[[name]],
      obs_seed = seed
    )
  }

  list(
    setup = setup,
    datasets = datasets
  )
}
