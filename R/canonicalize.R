#' @title Canonicalize imuGAP Data Objects
#'
#' @description
#' These functions validate, clean, and convert raw user-supplied data structures
#' (locations, observations, and populations) into the canonical forms required
#' by the `[sampling()]` sampler and the underlying Stan models.
#'
#' @details
#' The `imuGAP` hierarchical modeling framework requires data structures to adhere to
#' specific relational and format constraints. The three canonicalize functions
#' process and validate these inputs as described below:
#'
#' ## Locations (`canonicalize_locations`)
#' The `[sampling()]` sampler works on a hierarchical model of locations,
#' and must be provided that structure. This method checks location structure
#' validity, and returns a canonical version including the layer membership.
#'
#' A valid structure has:
#' - a unique root,
#' - no cycles, and
#' - no duplicate `loc_id`s
#'
#' Users may explicitly identify the root `loc_id` by providing a row with
#' `parent_id` equal to `NA`. Otherwise, any `parent_id` that does not appear
#' in `loc_id` is treated as the root.
#'
#' If the input is valid, this method will create the canonicalized version.
#' In that version, all ids run from 1:N, where N is the number of distinct
#' ids. That order is determined by layer order, then position of parent
#' within its layer, then "natural" order (i.e., whatever base R `sort()`
#' yields).
#'
#' ## Observations (`canonicalize_observations`)
#' The observations object documents observations used to fit the
#' model. Conceptually, each row represents an observation of vaccination status
#' within a population. That population need not be uniform
#' (see `[canonicalize_populations()]`) or concerning a single cohort or time:
#' each observation should generally be the best available resolution data. That
#' resolution can vary across rows. The sampler uses information
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
#' `NA`, and any right-censored observations with `1`. Note that `0` is *not* a
#' valid value at this time; we are preserving that for potential future support
#' of left-censoring.
#'
#' ## Populations (`canonicalize_populations`)
#' This method validates the meta-data associated with the observations, as well
#' as converting that meta-data to use the canonical id formats.
#'
#' Regarding "cohorts" and "ages": these are counted from 1, by 1 "unit". You
#' can imagine the units are whatever resolution is appropriate for your data:
#' months, quarters, years, etc. As long as these are used consistently,
#' estimation will work, and take on the unit meaning you used for input.
#'
#' @param locations a `[data.frame()]`, with columns `loc_id` and `parent_id`,
#'   of the same type. See Details for restrictions.
#' @param observations a `[data.frame()]`, the observed data, with at least
#'   three columns:
#'   - an `obs_id` column; any type, as long as unique, non-NA
#'   - a `positive` column; non-negative integers, the observed number of
#'     vaccinated individuals
#'   - a `sample_n` column; positive integers, the number of individuals
#'     sampled, must be greater than or equal to "positive"
#'   - optionally, a `censored` column; numeric, NA (uncensored) or 1
#'     (right-censored); if not present, will be assumed NA
#' @param populations a `[data.frame()]`, the observation meta data, with
#'   columns
#'  - `obs_id`, any type; the observation the row concerns (i.e. id shared with
#'    an observations data object)
#'  - `loc_id`, any type; the location the row concerns (i.e. id shared with a
#'    locations data object)
#'  - `dose`, a non-zero, positive integer (1, 2, ...); what dose row concerns
#'  - `cohort`, a positive integer; the cohort at the location row concerns
#'  - `age`, a positive integer; the age of that cohort row concerns
#'  - `weight`, a numeric, (0, 1); the relative contribution of this row to an
#'    observation. Optional if each population row has a unique `obs_id`.
#' @param drop_extra a logical scalar; drop extraneous columns? (default: yes)
#' @param max_cohort if present, what is the maximum cohort that should be
#'   present?
#' @param max_age if present, what is the maximum age that should be present?
#' @param max_dose maximum dose number to allow (default: 2L)
#'
#' @name canonicalize
#' @aliases canonicalize_locations canonicalize_observations canonicalize_populations
#' @importFrom data.table setkey data.table setattr
#' @global .
#' @autoglobal
NULL

#' @importFrom data.table setattr
#' @keywords internal
mark_canonical <- function(x, target_class) {
  setattr(x, "imuGAP-canonical", target_class)
  x[]
}

#' @keywords internal
is_canonical <- function(dt, target_class) {
  canonical <- attr(dt, "imuGAP-canonical", exact = TRUE)
  !is.null(canonical) && (canonical == target_class)
}

#' @rdname canonicalize
#' @return `canonicalize_locations` returns a `data.table`, with:
#'  - `loc_id`, `parent_id` columns as originally supplied, possibly reordered
#'  - `loc_c_id`, `loc_cp_id` columns, canonicalized id/parent_id columns,
#'    representing the order that will be used in the sampler
#'  - `layer` column, an integer from 1 (root), 2 (root children),
#'    3 (grandchildren), &c
#'  - `layer_bound` column, an integer starting from 1 by layer. This provides
#'    index slice information used in the stan model.
#'
#' @examples
#' # --- canonicalize_locations ---
#' data("locations_sim")
#' locations_sim
#' canonicalize_locations(locations_sim)
#' # can also be provided in non-canonical order, and with an implicit root
#' weird_locations <- subset(locations_sim, !is.na(parent_id))[
#'   sample(nrow(locations_sim) - 1L)
#' ]
#' canonicalize_locations(weird_locations)
#' @autoglobal
#' @export
canonicalize_locations <- function(locations) {
  # if already canonical, return
  if (is_canonical(locations, "locations")) {
    return(locations[])
  }

  locations <- data.table::as.data.table(locations)

  # Check that locations has required structure
  checked_cols(locations, c("loc_id", "parent_id"), warn_extra = TRUE)

  # check for duplicate ids
  if (length(dupes <- locations[, which(duplicated(loc_id))])) {
    stop(
      "locations$loc_id must be unique; found ",
      length(dupes),
      " duplicates: ",
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
      if (length(potential_root) > 0) {
        paste0(
          ": ",
          toString(potential_root, width = 80)
        )
      }
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

  mark_canonical(locations, "locations")
}

#' @rdname canonicalize
#' @return `canonicalize_observations` returns a canonical observation object,
#'   a `[data.table()]` with:
#'  - an `obs_c_id` column, an integer sequence from 1; the order observations
#'    will be passed to estimation
#'  - the original `obs_id` column, possibly reordered
#'  - `positive` and `sample_n` columns, possibly reordered
#'  - a "censored" column; all NA, if not present in original `observations`
#'    argument
#'
#' @examples
#' # --- canonicalize_observations ---
#' data("observations_sim")
#' observations_sim
#' canonicalize_observations(observations_sim)
#' @autoglobal
#' @export
canonicalize_observations <- function(observations, drop_extra = TRUE) {
  # if already canonical, return
  if (is_canonical(observations, "observations")) {
    return(observations[])
  }

  observations <- data.table::as.data.table(observations)
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
      length(dupes),
      " duplicates: ",
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
      obs_c_id,
      positive,
      sample_n,
      censored,
      obs_id
    )]
  }

  mark_canonical(observations, "observations")
}

#' @rdname canonicalize
#' @return `canonicalize_populations` returns a canonical populations object,
#'   mirroring the input `populations`,
#' with the following updates:
#' - `obs_c_id`, the observation id the row concerns, canonicalized to match
#'   the canonical observation ids
#' - `loc_c_id`, the location id the row concerns, canonicalized to match
#' - reordered to `obs_c_id` order
#'
#' @examples
#' # --- canonicalize_populations ---
#' data("populations_sim"); data("locations_sim"); data("observations_sim")
#' populations_sim
#' canonicalize_populations(populations_sim, observations_sim, locations_sim)
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
  populations <- data.table::as.data.table(populations)

  checked_cols(
    populations,
    c("obs_id", "loc_id", "cohort", "age", "dose")
  )

  if (!"weight" %in% names(populations)) {
    if (any(duplicated(populations$obs_id))) {
      stop(
        "'populations' is missing the following required column(s): weight"
      )
    }
    populations[, weight := 1.0]
  }

  observations <- canonicalize_observations(observations)
  locations <- canonicalize_locations(locations)

  checked_subset(populations, "dose", seq_len(max_dose))

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

  mark_canonical(populations, "populations")
}

#' @title Canonicalize a target grid against a fit
#'
#' @description
#' Normalizes a target grid and validates it against a specific `imugap_fit`.
#' Accepts either the output of `[create_target()]` or a plain `data.frame` /
#' `data.table` with `loc_id`, `age`, `cohort`, and `dose` columns (optionally
#' `weight` / `obs_id`). Fills `obs_c_id` and `weight` when absent, checks that
#' every `loc_id` exists in the fit and that `dose`, `age`, and `cohort` are within
#' the fit's ranges, and adds the canonical `loc_c_id`. Errors on any out-of-range
#' value. `[predict.imugap_fit()]` calls this internally, so most users do not call
#' it directly.
#'
#' @param fit an `imugap_fit` object returned by `[sampling()]`.
#' @param target a target grid: the output of `[create_target()]`, or a
#'   `data.frame` / `data.table` with `loc_id`, `age`, `cohort`, and `dose` columns.
#'
#' @return the validated `target` (a `data.table`) with `loc_c_id` added.
#'
#' @seealso `[create_target()]`, `[predict.imugap_fit()]`
#'
#' @examples
#' data("fit_sim")
#' target <- create_target(
#'   location = c("Blue Heron School", "Bluebird Learning Center"),
#'   age = c(1, 2, 3), cohort = 5, dose = c(1), mode = "snapshot"
#' )
#' canonicalize_target(fit_sim, target)
#'
#' @importFrom data.table as.data.table between
#' @autoglobal
#' @export
canonicalize_target <- function(fit, target) {
  target <- data.table::as.data.table(target)
  checked_cols(target, c("loc_id", "age", "cohort", "dose"))

  # obs_c_id: fill sequentially when absent, else require it to be 1:nrow.
  if (!"obs_c_id" %in% names(target)) {
    target[, obs_c_id := seq_len(.N)]
  } else if (!all(target$obs_c_id == seq_len(nrow(target)))) {
    stop("if supplied, obs_c_id must be 1:nrow(target)", call. = FALSE)
  }

  # obs_id: if supplied, must be unique and not NA.
  if (
    "obs_id" %in% names(target) &&
      (anyDuplicated(target$obs_id) > 0L || anyNA(target$obs_id))
  ) {
    stop("if supplied, obs_id must be unique and not NA", call. = FALSE)
  }

  # weight: default to 1, else require all 1.
  if (!"weight" %in% names(target)) {
    target[, weight := 1.0]
  } else if (!all(target$weight == 1.0)) {
    stop("if supplied, weight must be 1", call. = FALSE)
  }

  # --- validate against the fit ---
  invalid_locs <- setdiff(target$loc_id, fit$locations$loc_id)
  if (length(invalid_locs) > 0) {
    stop(
      "all locations must be within fit$locations. Invalid locations: ",
      toString(invalid_locs, width = 60),
      call. = FALSE
    )
  }

  invalid_dose_rows <- target[, which(!between(dose, 1L, fit$data$n_doses))]
  if (length(invalid_dose_rows) > 0) {
    stop(
      sprintf(
        "dose values must be within 1 and fit$data$n_doses (%i). Invalid dose in rows: ",
        fit$data$n_doses
      ),
      toString(invalid_dose_rows, width = 60),
      call. = FALSE
    )
  }

  invalid_age_rows <- target[, which(!between(age, 1L, fit$data$n_yr))]
  if (length(invalid_age_rows) > 0) {
    stop(
      sprintf(
        "age values must be within 1 and fit$data$n_yr (%i). Invalid age in rows: ",
        fit$data$n_yr
      ),
      toString(invalid_age_rows, width = 60),
      call. = FALSE
    )
  }

  invalid_cohort_rows <- target[, which(!between(cohort, 1L, fit$data$n_cohort))]
  if (length(invalid_cohort_rows) > 0) {
    stop(
      sprintf(
        "cohort values must be within 1 and fit$data$n_cohort (%i). Invalid cohort in rows: ",
        fit$data$n_cohort
      ),
      toString(invalid_cohort_rows, width = 60),
      call. = FALSE
    )
  }

  # keep the canonical columns in order, then add loc_c_id from the fit.
  cols <- c("obs_c_id", "loc_id", "age", "cohort", "dose", "weight")
  if ("obs_id" %in% names(target)) {
    cols <- c(cols, "obs_id")
  }
  target <- target[, .SD, .SDcols = cols]
  target[fit$locations, on = .(loc_id), loc_c_id := loc_c_id]
  target[]
}
