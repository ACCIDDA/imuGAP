
#' @title Validate `obs_population` object for `imuGAP`
#'
#' @inheritParams data.table::setDT
#' @param max_obs_id optional positive integer, the maximum valid observation id
#' @param max_location optional positive integer, the maximum valid location id
#' @param max_cohort optional positive integer, the maximum valid cohort id
#' @param max_age optional positive integer, the maximum valid age
#' @param subset logical; if `FALSE` (default), observations must be complete.
#' If `TRUE`, `max_obs_id` must be provided.
#'
#' @details
#'
#' A valid `obs_population` object must:
#'  - be convertible to a `data.table` via [data.table::setDT()]
#'  - contain the columns "obs_id", "location", "cohort", "age", "dose", and
#'  "weight"
#'  - obs_id, integers >= 1; if `max_obs_id` is provided, <= `max_obs_id`;
#'    if `subset` is `FALSE`, all integers 1:`max_obs_id` must be present
#'  - location must be a positive integer less than or equal to `max_location`.
#'  however, not all locations need to be represented in `wts`
#'  - cohort must be a positive integer
#'  - age must be a positive integer
#'  - dose must be 1 or 2
#'  - weight must be a positive numeric value; weights must sum to 1 by obs_id
#'
#' @importFrom data.table setDT
#'
#' @return if `x` is valid, returns the canonical `obs_population` object
#'
#' @section Errors:
#' If `x` is not valid, raises an error describing the issue.
#'
#' @export
check_obs_population <- function(
  x,
  max_obs_id, max_location, max_cohort, max_age,
  subset = !missing(max_obs_id)
) {

  # structural checks

  # correct class
  checked_dt_able(x)
  checked_cols(x, c("obs_id", "location", "cohort", "age", "dose", "weight"))
  checked_maxed_pos_integer(x, obs_id, max_obs_id)
  # also need to check that all obs_id 1:max_obs_id are all present

  checked_maxed_pos_integer(x, location, max_location)
  checked_maxed_pos_integer(x, cohort, max_cohort)
  checked_maxed_pos_integer(x, age, max_age)

  if (!all(x$dose %in% 1:2)) {
    stop("'wts$dose' must be 1 or 2")
  }

  if (any(!is.numeric(x$weight), any(x$weight <= 0))) {
    stop("'wts$weight' must be a positive numeric")
  }

  tol <- 1e-10

  if (x[, .(err = abs(sum(weight) - 1.0) >= tol), by = obs_id][, any(err)]) {
    stop("'wts$weight' must sum to 1 by 'obs_id'")
  }

  return(x)
}

#' @title Check location data
#'
#' @param locations a `data.frame`, with integer columns `id` and `parent_id`.
#' See Details for restrictions.
#'
#' @details
#' [imuGAP()] works on a hierarchical model of locations, so needs to be
#' told what that structure is. This method checks that the location structure
#' is valid, and returns a canonical version including the layer size.
#'
#' `id` may not have an duplicates. If there is a row with `id == 1`, it must
#' have `parent_id == NA`. `max(id)` must equal the number of rows, give or
#' take the presence of the root `id == 1` - put another way, `id` must be
#' `seq_len(dim(locations)[1])` (again, give or take 1 for the root node).
#' After ordering, `id` and `parent_id` must be `all(diff(id) == 1)` and
#' `all(diff(parent_id) %in% c(0, 1))`.
#'
#' Intended future capability: be able to extract a location structure that
#' satisfies the requirements. In particular:
#'  - allow a root other than 1; would be defined by a `parent_id` == `NA`
#'  - accept arbitrary column types for `id`, including a non-covering set of
#'  integers, and `parent_id`; the return object will include an appropriate
#'  remapping to be applied to weighting data
#'  - accept arbitrary layer depths
#'
#' @return a `data.table`, with the canonical location structure, and a
#' `layer` column
#' @importFrom data.table setkey data.table
#' @export
check_locations <- function(location_map) {
  # locations should be a data.frame of `id`, `parent_id`

  rev_dt <- checked_dt_able(
    eval(substitute(location_map)), TRUE
  )[, .(id, parent_id)]
  checked_cols(rev_dt, c("id", "parent_id"))

  if (rev_dt[is.na(parent_id), .N > 1]) stop("multiple parent_id == NA")
  setkey(rev_dt, id, parent_id)
  if (rev_dt[1, !(id %in% c(1, 2))]) stop("starts with id other than 1 or 2")

  if (rev_dt[!is.na(parent_id), any(id <= parent_id)]) stop("id <= parent_id")
  if (rev_dt[, any(diff(id) != 1)]) stop("missing or extra id")
  if (rev_dt[!is.na(parent_id), !all(diff(parent_id) %in% c(0, 1))]) stop("missing or extra id")
  if (rev_dt[!is.na(parent_id)][1, parent_id != 1]) stop("must start w/ parent_id == 1")

  if (!rev_dt[is.na(parent_id), .N]) {
    rev_dt <- rbind(
      data.table(id = 1, parent_id = NA_integer_, layer = 1L, layer_bound = 1L),
      rev_dt, fill = TRUE
    )
  }

  onlayer <- 1L
  while(rev_dt[, any(is.na(layer))]) {
    parents <- rev_dt[layer == onlayer, id]
    onlayer <- onlayer + 1L
    rev_dt[parent_id %in% parents, layer := onlayer]
    rev_dt[layer == onlayer, layer_bound := seq_len(.N)]
    rev_dt[, layer_bound := min(layer_bound), by = parent_id]
  }

  return(rev_dt[])
}

#' @title Check observations objects
#'
#' @param observations a `data.frame` with:
#'  - nonnegative integer `positive` column
#'  - positive integer `sample_n` column, which must be greater than or equal
#'    to the `positive` column
#'  - optionally, an `id` column, 1:size(observations)
#'
#' @return the observation object, possibly changed in place to cast columns
#'  to integers and add the `id` column if not original present. Will error
#'  if the column requirements are not satisfied.
#'
#' @export
#' @importFrom data.table as.data.table setkey
check_observations <- function(observations) {
  observations <- as.data.table(observations)
  checked_cols(observations, c("positive", "sample_n"))
  checked_nonneg_integer(observations, "positive")
  checked_positive_integer(observations, "sample_n")
  if (observations[, any(positive > sample_n)]) stop("positive must be <= sample_n")

  if ("id" %in% names(observations)) {
    checked_positive_integer(observations, "id")
    setkey(observations, id)
    if (!all(seq_len(observations[, .N]) == observations$id)) {
      stop("if id provided, must be unique 1:.N")
    }
  } else {
    observations[, id := seq_len(.N)]
  }
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
#' @inheritParams check_observations
#' @inheritParams check_locations
#' @param max_cohort if present, what is the maximum cohort that should be present?
#' @param max_age if present, what is the maximum age that should be present?
check_obs_population <- function(
  obs_population,
  observations,
  locations,
  max_cohort,
  max_age
) {

  # using internal methods, check that obs_population has the correct structure
  checked_dt_able(obs_population)
  checked_cols(obs_population, c("obs_id", "location", "cohort", "age", "dose", "weight"))

  checked_set_equivalence(
    obs_population, "dose", c(1L, 2L)
  )

  # check that obs_population obs_ids all match observation ids; assumes observations
  # has already passed check_observations
  checked_set_equivalence(
    obs_population, "obs_id", observations$id
  )

  # check that obs_population loc_ids are all within locations ids; assumes locations
  # has already passed check_locations
  checked_subset(
    obs_population, "location", unique(c(locations$id, locations$parent_id))
  )

  # check cohort and age if max values provided
  checked_maxed_pos_integer(obs_population, "cohort", max_cohort)
  checked_maxed_pos_integer(obs_population, "age", max_age)

  # check that weight is a positive numeric
  if (obs_population[, any(!is.numeric(weight) | weight <= 0)]) {
    stop("'obs_population$weight' must be a positive numeric")
  }

  # check that weights sum to 1 by obs_id
  wt_check <- obs_population[, .(err = abs(sum(weight) - 1.0) >= 1e-8), by = obs_id]
  if (any(wt_check$err)) {
    stop("obs_population$weight must sum to 1 by obs_id")
  }

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
#' @param observations a `data.frame`, the observed data, with at least two columns:
#'   - a column named "positive", or the first column; a non-negative integer, the observed number of vaccinated individuals
#'   - a column named "sample_n", or the second column: a positive integer, the number of individuals sampled
#'   - if there exists a column "id", it must be unique, and will be used as the primary key for the observation data;
#'     otherwise, a unique id will be created based on the row number
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
  observations, obs_populations,
  locations, dose_schedule,
  imugap_opts = imugap_options(),
  stan_opts = stan_options()
) {

  # check location argument
  loc_info <- check_locations(locations)
  n_cnty <- loc_info[layer == 2, .N]
  n_schl <- loc_info[layer == 3, .N]

  # check observations argument
  obs <- check_observations(observations)[, .(id, positive, sample_n)]

  # check obs_populations - confirm wts locations
  wts <- check_obs_population(
    obs_populations, obs, loc_info
  )

  bsp <- splines::bs(
    seq_len(wts[, diff(range(cohort)) + 1L]),
    df = imugap_opts$df, intercept = TRUE
  )

  doses <- matrix(0, ncol  = length(dose_schedule), nrow = max(wts$age))
  for (i in seq_along(dose_schedule)) {
    doses[(dose_schedule[i] + 1):nrow(doses), i] <- 1
  }
 
  # prepare dat_stan
  dat_stan <- list(
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

  stan_opts$data <- dat_stan
  out <- do.call(rstan::sampling, stan_opts)
  return(out)
}
