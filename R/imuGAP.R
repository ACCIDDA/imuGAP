#' @importFrom data.table setattr
#' @keywords internal
mark_canonical <- function(x, target_class) {
  setattr(x, "imuGAP-canonical", target_class)
  return(x[])
}

#' @keywords internal
is_canonical <- function(dt, target_class) {
  canonical <- attr(dt, "imuGAP-canonical", exact = TRUE)
  return(!is.null(canonical) && (canonical == target_class))
}

#' @title Canonicalize Location Data
#'
#' @param locations a `[data.frame()]`, with columns `loc_id` and `parent_id`,
#'   of the same type. See Details for restrictions.
#'
#' @details The `[imuGAP()]` sampler works on a hierarchical model of locations,
#'   and must be provided that structure. This method checks location structure
#'   validity, and returns a canonical version including the layer membership.
#'
#'   A valid structure has:
#'  - a unique root,
#'  - no cycles, and
#'  - no duplicate `loc_id`s
#'
#'   Users may explicitly identify the root `loc_id` by providing a row with
#'   `parent_id` equal to `NA`. Otherwise, any `parent_id` that does not appear
#'   in `loc_id` is treated as the root.
#'
#'   If the input is valid, this method will create the canonicalized version.
#'   In that version, all ids run from 1:N, where N is the number of distinct
#'   ids. That order is determined by layer order, then position of parent
#'   within its layer, then "natural" order (i.e., whatever base R [sort()]
#'   yields).
#'
#' @return a `data.table`, with:
#'  - `loc_id`, `parent_id` columns as originally supplied, possibly reordered
#'  - `loc_c_id`, `loc_cp_id` columns, canonicalized id/parent_id columns,
#'    representing the order that will be used in the sampler
#'  - `layer` column, an integer from 1 (root), 2 (root children),
#'    3 (grandchildren), &c
#'  - `layer_bound` column, an integer starting from 1 by layer. This provides
#'    index slice information used in the stan model.
#'
#' @importFrom data.table as.data.table setkey data.table setattr
#' @global .
#' @autoglobal
#' @export
canonicalize_locations <- function(locations) {

  # if already canonical, return
  if (is_canonical(locations, "locations")) return(locations[])

  locations <- as.data.table(locations)

  # Check that locations has required structure
  checked_cols(locations, c("loc_id", "parent_id"), warn_extra = TRUE)

  # check for duplicate ids
  if (length(dupes <- locations[, which(duplicated(loc_id))])) {
    stop(
      "locations$loc_id must be unique; found ",
      length(dupes), " duplicates: ",
      toString(dupes, width = 80)
    )
  }

  # Find candidate unique root
  potential_root <- locations[is.na(parent_id), loc_id] |> unique()
  if (!length(potential_root)) {
    # if no explicit root, find implicit root
    potential_root <- setdiff(locations$parent_id, locations$loc_id) |> unique()
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

  # if root is implicit, add it to the data
  if (!locations[loc_id == potential_root, .N]) {
    locations <- rbind(
      data.table(loc_id = potential_root),
      locations,
      fill = TRUE
    )
  }

  # recursively assign layer membership, starting with root
  on_layer <- 1L
  locations$layer <- NA_integer_
  locations[is.na(parent_id), layer := on_layer]
  layer_members <- locations[layer == on_layer, loc_id]

  while (locations[, any(is.na(layer))]) {
    on_layer <- on_layer + 1L
    locations[parent_id %in% layer_members, layer := on_layer]
    layer_members <- locations[layer == on_layer, loc_id]
    # check for cycles - if we have not assigned any new layer members,
    # but still have unassigned locations, then we have a cycle
    if (length(layer_members) == 0L && locations[, any(is.na(layer))]) {
      stop(
        "locations may not contain cycles; found ",
        locations[is.na(layer), .N],
        " ids in cycle(s). Offending locations: ",
        toString(locations[is.na(layer), loc_id], width = 80)
      )
    }
  }

  # canonicalize ids, by layer, then parent position, then natural order

  setkey(locations, layer, parent_id, loc_id)
  locations[, loc_c_id := seq_len(.N)]
  locations[
    locations[, .(parent_id = loc_id, loc_cp_id = loc_c_id)],
    on = .(parent_id),
    loc_cp_id := loc_cp_id
  ]

  locations[, layer_bound := seq_len(.N), by = layer]
  locations[, layer_bound := min(layer_bound), by = loc_cp_id]

  return(mark_canonical(locations, "locations"))
}

#' @title Canonicalize Observation Data
#'
#' @param observations a `[data.frame()]`, the observed data, with at least
#'   three columns:
#'   - an `obs_id` column; any type, as long as unique, non-NA
#'   - a `positive`` column; non-negative integers, the observed number of
#'     vaccinated individuals
#'   - a `sample_n` column; positive integers, the number of individuals
#'     sampled, must be greater than or equal to "positive"
#'   - optionally, a `censored` column; numeric, NA (uncensored) or 1
#'     (right-censored); if not present, will be assumed NA
#' @param drop_extra a logical scalar; drop extraneous columns? (default: yes)
#'
#' @details The observations object documents observations used to fit the
#' model. Conceptually, each row represents an observation of vaccination status
#' within a population. That population need not be uniform
#' (see `[canonicalize_populations()]`) or concerning a single cohort or time:
#' each observation should generally be the best available resolution data. That
#' resolution can vary across rows. The `[imuGAP()]` sampler uses information
#' about the resolutions to automatically figure out how to compare the latent
#' process model to those different observations.
#'
#' For the optional `censored` column: the model supports vaccination status
#' indicators which are vaccine specific as well as those which represent an
#' individual having all of a set of vaccines (including the target vaccine).
#' The specific coverage for the target vaccine is right-censored in the latter
#' case: full-set-coverage is the minimum coverage for the target.
#'
#' When at least some of the data are censored, you must supply the `censored`
#' column to correctly estimate coverage. Mark any uncensored observations with
#' NA, and any right-censored observations with $1$. Note that $0$ is *not* a
#' valid value at this time; we are preserving that for potential future support
#' of left-censoring.
#'
#' @return a canonical observation object, a `[data.table()]` with:
#'  - an `obs_c_id` column, an integer sequence from 1; the order observations
#'    will be passed to estimation
#'  - the original `obs_id` column, possibly reordered
#'  - `positive` and `sample_n` columns, possibly reordered
#'  - a "censored" column; all NA, if not present in original `observations`
#'    argument
#'
#' @export
#' @importFrom data.table as.data.table setkey
#' @autoglobal
canonicalize_observations <- function(observations, drop_extra = TRUE) {

  # if already canonical, return
  if (is_canonical(observations, "observations")) {
    return(observations[])
  }

  observations <- as.data.table(observations)
  checked_cols(observations, c("obs_id", "positive", "sample_n"))

  # check id column validity
  if (observations[, any(is.na(obs_id))]) {
    stop(
      "obs_id column may not contain NA; found ",
      observations[is.na(obs_id), .N],
      " NA values at rows: ",
      toString(observations[, which(is.na(obs_id))]),
      width = 80
    )
  }

  if (length(dupes <- observations[, which(duplicated(obs_id))])) {
    stop(
      "observations$obs_id must be unique; found ",
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
      toString(observations[positive > sample_n, obs_id], width = 80)
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

  # canonicalize ids: order by censoring, then original obs_id.
  # this results in uncensored then censored observations.
  setkey(observations, censored, obs_id)
  observations[, obs_c_id := seq_len(.N)]

  if (drop_extra) {
    observations <- observations[, .(
      obs_c_id, positive, sample_n, censored, obs_id
    )]
  }

  return(mark_canonical(observations, "observations"))
}

#' @title Check observation meta data object
#'
#' @param populations a `[data.frame()]`, the the observation meta data, with
#'   columns
#'  - `obs_id`, any type; the observation the row concerns (i.e. id shared with
#'    an observations data object)
#'  - `loc_id``, any type; the location the row concerns (i.e. id shared with a
#'    locations data object)
#'  - `dose`, a non-zero, positive integer (1, 2, ...); what dose row concerns
#'  - `cohort`, a positive integer; the cohort at the location row concerns
#'  - `age`, a positive integer; the age of that cohort row concerns
#'  - `weight`, a numeric, (0, 1); the relative contribution of this row to an
#'    observation
#'   Note that multiple rows may concern the same observation, meaning that the
#'   populations from different cohorts, locations, and ages may be pooled in an
#'   observation
#' @inheritParams canonicalize_observations
#' @inheritParams canonicalize_locations
#' @param max_cohort if present, what is the maximum cohort that should be
#'   present?
#' @param max_age if present, what is the maximum age that should be present?
#'
#' @details
#' This method validates the meta-data associated with the observations, as well
#' as converting that meta-data to use the canonical id formats.
#'
#' Regarding "cohorts" and "ages": these are counted from 1, by 1 "unit". You
#' can imagine the units are whatever resolution is appropriate for your data:
#' months, quarters, years, etc. As long as these are used consistently,
#' estimation will work, and take on the unit meaning you used for input.
#'
#' @return a canonical populations object, mirroring the input `populations`,
#' with the following updates:
#' - `obs_c_id`, the observation id the row concerns, canonicalized to match
#'   the canonical observation ids
#' - `loc_c_id`, the location id the row concerns, canonicalized to match
#' - reordered to `obs_c_id` order
#'
#' @autoglobal
#' @export
canonicalize_populations <- function(
  populations,
  observations,
  locations,
  max_cohort,
  max_age,
  max_dose = 2L
) {

  if (is_canonical(populations, "populations")) {
    return(populations[])
  }

  # using internal methods, check that populations has the correct structure
  checked_dt_able(populations)
  checked_cols(
    populations, c("obs_id", "loc_id", "cohort", "age", "dose", "weight")
  )

  observations <- canonicalize_observations(observations)
  locations <- canonicalize_locations(locations)

  checked_subset(populations, "dose", c(1L, 2L))

  # check that populations id correspond to all observation ids
  checked_set_equivalence(populations, "obs_id", observations$obs_id)

  # check that populations locations are all *within* locations ids;
  checked_subset(populations, "loc_id", locations$loc_id)

  # check cohort and age if max values provided
  checked_maxed_pos_integer(populations, "cohort", max_cohort)
  checked_maxed_pos_integer(populations, "age", max_age)

  # check that weight is a positive numeric; > 1 weights caught in next block
  if (populations[, any(!is.numeric(weight) | weight <= 0)]) {
    stop("'populations$weight' must be a positive numeric")
  }

  # check that weights sum to 1 by obs_id
  wt_check <- populations[,
    .(err = abs(sum(weight) - 1.0) >= 1e-8),
    by = obs_id
  ]
  if (any(wt_check$err)) {
    stop("populations$weight must sum to 1 by obs_id")
  }

  # introduce canonical id
  populations[observations, on = .(obs_id), obs_c_id := obs_c_id]
  populations[locations, on = .(loc_id), loc_c_id := loc_c_id]

  setkey(populations, obs_c_id, loc_c_id, cohort, age, dose)

  populations[, range_start := seq_len(.N)]
  populations[, range_start := min(range_start), by = obs_c_id]

  return(mark_canonical(populations, "populations"))
}

# if predict == TRUE, then need to provide max life year to predict out to

#' @title Stan Sampler Options
#'
#' @description
#' This function encapsulates option passing to the stan sampler, with the
#' exception of the model object, which is passed in `imugap_options`.
#'
#' @inheritDotParams rstan::sampling
#' @inheritParams rstan::sampling
#'
#' @return a list of arguments matching [rstan::sampling()] inputs
#' @export
stan_options <- function(...) {
  res <- list(...)
  if ("object" %in% names(res)) {
    stop(
      "Passing 'object' in stan_options is not allowed; ",
      "The model object should be passed in `imugap_options` instead."
    )
  }
  return(res)
}

#' @title imuGAP Model Options
#'
#' @description
#' This function encapsulates option passing for imuGAP settings.
#'
#' @param df degrees of freedom to use in bspline
#' @param dose_schedule an integer vector, the ages at which dose(s) `n` are
#'   scheduled, with vector indices and doses matching
#' @param object which stan model object to use; currently only "default" is
#'   supported
#'
#' @return a list of imuGAP model options
#' @export
imugap_options <- function(
  df = 5L, dose_schedule = c(1, 4),
  object = c("default")
) {
  object <- switch(object,
    "default" = stanmodels$impute_school_coverage_process_v6,
    stop("Unknown model object: ", object)
  )
  return(as.list(environment()))
}

#' @title Immunity: Geographic & Age-based Projection, `imuGAP`
#'
#' @description
#' This a sampler interface to convert user-friendly data into the necessary
#' format to feed the immunity estimation model.
#'
#' @inheritParams canonicalize_observations
#' @inheritParams canonicalize_populations
#' @inheritParams canonicalize_locations
#' @param imugap_opts options for the `imuGAP` model
#' @param stan_opts passed to `rstan::sampling` (e.g. `iter`, `chains`).
#'
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
#' @autoglobal
#' @export
imuGAP <- function( # nolint
  observations,
  populations,
  locations,
  imugap_opts = imugap_options(),
  stan_opts = stan_options()
) {

  # check location argument
  loc_info <- canonicalize_locations(locations)
  layer_sizes <- loc_info[, .N, keyby = layer][, c(N)]
  if (length(layer_sizes) != 3) {
    stop(
      "imuGAP currently only supports 3-layer models ",
      "(e.g. single state => counties => schools); offered ",
      length(layer_sizes),
      " layers."
    )
  }

  n_cnty <- layer_sizes[2]
  n_schl <- layer_sizes[3]

  # check observations argument
  obs <- canonicalize_observations(observations)

  # check populations - confirm wts locations
  wts <- canonicalize_populations(
    populations,
    obs,
    loc_info
  )

  bsp <- splines::bs(
    seq_len(wts[, diff(range(cohort)) + 1L]),
    df = imugap_opts$df,
    intercept = TRUE
  )

  dose_schedule <- imugap_opts$dose_schedule

  doses <- matrix(0, ncol = length(dose_schedule), nrow = max(wts$age))
  for (i in seq_along(dose_schedule)) {
    doses[(dose_schedule[i] + 1):nrow(doses), i] <- 1
  }

  # prepare dat_stan
  stan_opts$data <- list(
    n_uncensored_obs = obs[is.na(censored), .N],
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
    weights_school = wts$loc_c_id,
    weights_cohort = wts$cohort,
    weights_life_year = wts$age,
    weights_dose = wts$dose,
    weights = wts$weight,
    n_cnty = n_cnty,
    cnty_bounds = loc_info[layer == 3, unique(layer_bound)],
    predict_mode = 0
  )
  stan_opts$object <- imugap_opts$object

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

#' @title Install imuGAP CLI to PATH
#'
#' @description Creates a symlink to the bundled CLI script so \code{imugap} is
#' available as a shell command.
#'
#' @param path character; directory to install the symlink into.
#'   Defaults to \code{"~/.local/bin"}.
#'
#' @return Invisible \code{TRUE} on success, errors on failure.
#'
#' @export
install_cli <- function(path = "~/.local/bin") {
  if (.Platform$OS.type == "windows") {
    stop("install_cli() is not supported on Windows.", call. = FALSE)
  }

  script <- system.file("scripts", "imugap.R", package = "imuGAP")
  if (!nzchar(script)) {
    stop("Cannot find bundled CLI script. Is imuGAP installed?", call. = FALSE)
  }

  path <- normalizePath(path, mustWork = FALSE)
  if (!dir.exists(path)) {
    stop("Directory does not exist: ", path, call. = FALSE)
  }

  link <- file.path(path, "imugap")

  if (interactive()) {
    ans <- readline(paste0("Install symlink at ", link, "? [Y/n] "))
    if (!tolower(ans) %in% c("", "y", "yes")) {
      message("Aborted.")
      return(invisible(FALSE))
    }
  }

  unlink(link)
  ok <- file.symlink(script, link)
  if (!ok) {
    stop("Failed to create symlink at ", link, call. = FALSE)
  }
  message("Installed: ", link, " -> ", script)
  message("Ensure ", path, " is on your PATH.")
  return(invisible(TRUE))
}
