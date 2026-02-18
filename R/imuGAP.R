check_processed <- function(dt) {

}

#' @title Canonicalize Location Data
#'
#' @param locations a `[data.frame()]`, with columns `id` and `parent_id`, of
#'   the same type. See Details for restrictions.
#'
#' @details The `[imuGAP()]` sampler works on a hierarchical model of locations,
#'   and must be provided that structure. This method checks location structure
#'   validity, and returns a canonical version including the layer membership.
#'
#'   A valid structure has:
#'  - a unique root,
#'  - no cycles, and
#'  - no duplicate `id`s
#'
#'   Users may explicitly identify the root `id` by providing a row with
#'   `parent_id` equal to `NA`. Otherwise, any `parent_id` that does not appear
#'   in `id` is treated as the root.
#'
#'   If the input is valid, this method will create the canonicalized version.
#'   In that version, all ids run from 1:N, where N is the number of distinct
#'   ids. That order is determined by layer order, then position of parent
#'   within its layer, then "natural" order (i.e., whatever base R [sort()]
#'   yields).
#'
#' @return a `data.table`, with:
#'  - "id", "parent_id" columns as originally supplied, possibly reordered
#'  - "c_id", "cp_id" columns, canonicalized id/parent_id columns,
#'    representing the order that will be used in the sampler
#'  - "layer" column, an integer from 1 (root), 2 (root children),
#'    3 (grandchildren), etc
#' @importFrom data.table as.data.table setkey data.table setattr
#' @export
#' @global .
#' @autoglobal
canonicalize_locations <- function(locations) {

  # if already canonical, return
  if (attr(locations, "imuGAP-canonical", exact = TRUE) == "locations") {
    return(locations[])
  }

  locations <- as.data.table(locations)

  # Check that locations has required structure
  checked_cols(locations, c("id", "parent_id"), warn_extra = TRUE)

  # Find candidate unique root
  potential_root <- locations[is.na(parent_id), id] |> unique()
  if (!length(potential_root)) {
    # if no explicit root, find implicit root
    potential_root <- setdiff(locations$parent_id, locations$id) |> unique()
  }

  # Error if not exactly one root
  if (length(potential_root) != 1L) {
    stop(
      "locations must have exactly one root, but found ",
      length(potential_root),
      if (length(potential_root) > 0) paste0(
        ": ", toString(potential_root, width = 80)
      )
    )
  }

  # check for duplicate ids
  if (length(dupes <- locations[, which(duplicated(id))])) {
    stop(
      "locations$id must be unique; found ",
      length(dupes), " duplicates: ",
      toString(dupes, width = 80)
    )
  }

  # if root is implicit, add it to the data
  if (!locations[id == potential_root, .N]) {
    locations <- rbind(
      data.table(id = potential_root),
      locations,
      fill = TRUE
    )
  }

  # recursively assign layer membership, starting with root
  on_layer <- 1L
  locations$layer <- NA_integer_
  locations[is.na(parent_id), layer := on_layer]
  layer_members <- locations[layer == on_layer, id]

  while (locations[, any(is.na(layer))]) {
    on_layer <- on_layer + 1L
    locations[parent_id %in% layer_members, layer := on_layer]
    layer_members <- locations[layer == on_layer, id]
    # check for cycles - if we have not assigned any new layer members,
    # but still have unassigned locations, then we have a cycle
    if (length(layer_members) == 0L && locations[, any(is.na(layer))]) {
      stop(
        "locations may not contain cycles; found ",
        locations[is.na(layer), .N],
        " ids in cycle(s). Offending locations: ",
        toString(locations[is.na(layer), id], width = 80)
      )
    }
  }

  # canonicalize ids, by layer, then parent position, then natural order

  setkey(locations, layer, parent_id, id)
  locations[, c_id := seq_len(.N)]
  locations[
    locations[, .(parent_id = id, cp_id = c_id)],
    on = .(parent_id),
    cp_id := cp_id
  ]

  setattr(locations, "imuGAP-canonical", "locations")

  return(locations[])
}

#' @title Canonicalize Observation Data
#'
#' @param observations a `[data.frame()]`, the observed data, with at least
#'   three columns:
#'   - an "id" column; any type, as long as unique, non-NA
#'   - a "positive" column; non-negative integers, the observed number of
#'     vaccinated individuals
#'   - a "sample_n" column; positive integers, the number of individuals sampled
#'   - optionally, a "censored" column; numeric, NA (uncensored) or 1
#'     (right-censored);
#'
#' @return a canonical observation object, a `[data.table()]` with:
#'  - an "obs_id" column, an integer sequence from 1; the order observations
#'    will be passed to estimation
#'  - the original "id" column, possibly reordered
#'  - "positive" and "sample_n" columns, possibly reordered
#'  - a "censored" column; all NA, if not present in original `observations`
#'    argument
#'
#' @export
#' @importFrom data.table as.data.table setkey
#' @autoglobal
canonicalize_observations <- function(observations) {

  # if already canonical, return
  if (attr(observations, "imuGAP-canonical", exact = TRUE) == "observations") {
    return(observations[])
  }

  observations <- as.data.table(observations)
  checked_cols(observations, c("id", "positive", "sample_n"))

  # check id column validity
  if (observations[, any(is.na(id))]) {
    stop(
      "id column may not contain NA; found ",
      observations[is.na(id), .N],
      " NA values at rows: ",
      toString(observations[, which(is.na(id))]), width = 80)
  }

  if (length(dupes <- observations[, which(duplicated(id))])) {
    stop(
      "observations$id must be unique; found ",
      length(dupes), " duplicates: ",
      toString(dupes, width = 80)
    )
  }

  # check scientific data validity
  checked_nonneg_integer(observations, "positive")
  checked_positive_integer(observations, "sample_n")

  if (observations[, any(positive > sample_n)]) {
    stop(
      "positive must be <= sample_n; found ",
      observations[positive > sample_n, .N],
      " invalid observations with offending ids: ",
      toString(observations[positive > sample_n, id], width = 80)
    )
  }


  if ("censored" %in% names(observations)) {
    # confirmed censored is numeric, and only contains NA or 1
    if (!is.numeric(observations$censored)) {
      stop("if provided, 'censored' column must be numeric")
    }
    if (observations[, any(!is.na(censored) & censored != 1)]) {
      stop(
        "if provided, 'censored' column must be NA (uncensored)",
        " or 1 (right-censored)"
      )
    }
  } else {
    observations[, censored := NA_real_]
  }

  # canonicalize ids: order by censoring
  setkey(observations, censored, id)
  observations[, obs_id := seq_len(.N)]

  setattr(observations, "imuGAP-canonical", "observations")

  return(observations[])
}

#' @title Check observation meta data object
#'
#' @param obs_population a data.frame, the the observation weighting object, with columns
#'  - obs_id, the observation id the row concerns
#'  - dose, which dose the row concerns
#'  - location, the location the row concerns
#'  - cohort, the cohort at that location the row concerns
#'  - age, the age of that cohort the row concerns
#'  - weight, the relative contribution of this row to an observation
#' Note that multiple rows may concern the same observation, meaning that the populations
#' from different cohorts, locations, and ages may be pooled in an observation
#' @inheritParams canonicalize_observations
#' @inheritParams canonicalize_locations
#' @param max_cohort if present, what is the maximum cohort that should be present?
#' @param max_age if present, what is the maximum age that should be present?
canonicalize_populations <- function(
  obs_population,
  observations,
  locations,
  max_cohort,
  max_age
) {
  # using internal methods, check that obs_population has the correct structure
  checked_dt_able(obs_population)
  checked_cols(
    obs_population,
    c("obs_id", "location", "cohort", "age", "dose", "weight")
  )

  checked_set_equivalence(
    obs_population,
    "dose",
    c(1L, 2L)
  )

  # check that obs_population obs_ids all match observation ids; assumes observations
  # has already passed canonicalize_observations
  checked_set_equivalence(
    obs_population,
    "obs_id",
    observations$id
  )

  # check that obs_population loc_ids are all within locations ids; assumes locations
  # has already passed canonicalize_locations
  checked_subset(
    obs_population,
    "location",
    unique(c(locations$id, locations$parent_id))
  )

  # check cohort and age if max values provided
  checked_maxed_pos_integer(obs_population, "cohort", max_cohort)
  checked_maxed_pos_integer(obs_population, "age", max_age)

  # check that weight is a positive numeric
  if (obs_population[, any(!is.numeric(weight) | weight <= 0)]) {
    stop("'obs_population$weight' must be a positive numeric")
  }

  # check that weights sum to 1 by obs_id
  wt_check <- obs_population[,
    .(err = abs(sum(weight) - 1.0) >= 1e-8),
    by = obs_id
  ]
  if (any(wt_check$err)) {
    stop("obs_population$weight must sum to 1 by obs_id")
  }

  setkey(obs_population, obs_id, location, cohort)

  obs_population[, range_start := seq_len(.N)]
  obs_population[, range_start := min(range_start), by = obs_id]

  return(obs_population[])
}


# Question: how do we want to handle filtering observations and/or obs_populations?
# basically what do we want to do about the tradeoff on convenience vs safety?
# option: provide a bool subset argument, which allows people to allow subset matching across obs and obs_pop

# change the direction of location ordering - go from lowest to highest

# move dose schedule to first-class argument

# if predict == TRUE, then need to provide max life year to predict out to

#' @title Options for stan sampler
#'
#' @description
#' This function encapsulates option passing to the stan sampler.
#'
#' @inheritDotParams rstan::sampling
#' @inheritParams rstan::sampling
#'
#' @return a list of arguments matching [rstan::sampling()] inputs
#' @export
stan_options <- function(
  object = stanmodels$impute_school_coverage_process_v5,
  ...
) {
  res <- list(...)
  res$object <- object
  return(res)
}

#' @title Options for imuGAP model
#'
#' @description
#' This function encapsulates option passing for imuGAP settings
#'
#' @param df degrees of freedom to use in bspline
#'
#' @return a list of imuGAP model options
#'
imugap_options <- function(df = 5L) {
  return(as.list(environment()))
}

#' @title Immunity: Geographic & Age-based Projection, `imuGAP`
#'
#' @description This function estimates current coverage, by age and location.
#'
#' @inheritParams canonicalize_observations
#' @param obs_populations a `data.frame` providing the meta-data about the observations. Briefly, it should capture for each observation
#' the relevant location(s), cohort(s), age(s), dose, and relative contribution of that population to the observation. Each
#' row in `obs` should have one or more corresponding rows in `wts`. The `wts` data.frame should have at least the following columns:
#'  - a column named "obs_id", with a value in the "id" column in `obs`
#'  - "location", a positive integer, distinguishing locations.
#'  - "cohort", a positive integer, distinguishing cohorts
#'  - "age", a positive integer, distinguishing when this observation applies for this location/cohort
#'  - "dose", a positive integer, which dose this observation applies to
#'  - "weight", a positive numeric value, representing the relative contribution of that population to the observation
#' @param locations the location hierarchy, a `data.frame` with two columns:
#'   - "id", a positive integer, the location id
#'   - "parent_id", a positive integer or `NA`, the parent location id; the root location should have `NA` as its parent_id
#' @param dose_schedule a numeric vector of length 2, the ages at which dose 1 and dose 2 are scheduled
#' @param imugap_opts options for the `imuGAP` model
#' @param stan_opts passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
#' @export
imuGAP <- function(
  observations,
  obs_populations,
  locations,
  dose_schedule,
  imugap_opts = imugap_options(),
  stan_opts = stan_options()
) {
  # check location argument
  loc_info <- canonicalize_locations(locations)
  n_cnty <- loc_info[layer == 2, .N]
  n_schl <- loc_info[layer == 3, .N]

  # check observations argument
  obs <- canonicalize_observations(observations)[, .(id, positive, sample_n)]

  # check obs_populations - confirm wts locations
  wts <- canonicalize_populations(
    obs_populations,
    obs,
    loc_info
  )

  bsp <- splines::bs(
    seq_len(wts[, diff(range(cohort)) + 1L]),
    df = imugap_opts$df,
    intercept = TRUE
  )

  doses <- matrix(0, ncol = length(dose_schedule), nrow = max(wts$age))
  for (i in seq_along(dose_schedule)) {
    doses[(dose_schedule[i] + 1):nrow(doses), i] <- 1
  }

  # prepare dat_stan
  stan_opts$data <- list(
    n_yr = max(wts$age),
    n_cohort = max(wts$cohort),
    n_sch = n_schl,
    n_doses = length(dose_schedule),
    dose_sched = doses,
    k_bs = ncol(bsp),
    bs = bsp,
    n_obs = nrow(obs),
    y_obs = obs$positive,
    y_smp = obs$sample_n,
    n_weights = nrow(wts),
    obs_to_weights_bounds = unique(wts$range_start),
    weights_school = wts$location,
    weights_cohort = wts$cohort,
    weights_life_year = wts$age,
    weights_dose = wts$dose,
    weights = wts$weight,
    n_cnty = n_cnty,
    cnty_bounds = loc_info[layer == 3, unique(layer_bound)],
    predict_mode = 0
  )

  return(do.call(rstan::sampling, stan_opts))
}

#' @title Custom imuGAP fit extraction
#'
#' @description
#' Thin wrapper around `rstan::extract` to extract typical imuGAP parameters.
#' @param fit a `stanfit` object returned by `imuGAP()`
#' @param pars character vector; parameters to extract.
#'
#' @return a list, as returned by `rstan::extract()`
#'
extract_imugap <- function(fit, pars = c("logit_phi_state"), ...) {
  return(rstan::extract(fit, pars = pars, ...))
}
