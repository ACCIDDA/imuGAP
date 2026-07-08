#' @title Create observation populations
#'
#' @description
#' `create_observation_populations` is a convenience function to construct a properly weighted
#' `populations` object for typical modes of observation.
#'
#' @param observations a pre- or post-canonicalization `observations` object.
#'   Optionally contains additional columns required for the specified `mode` that vary by row.
#' @param mode character; the mode for populations creation (default: "snapshot").
#' @param ... additional arguments determined by the specified `mode` requirements
#'   for values which do not vary by row.
#
#' @details
#' This function uses a combination of varying information from `observations` and
#' fixed information from `...` arguments to provide the necessary information
#' for the modes to produce a `[canonicalize_populations()]` ready-object.
#'
#' Supported modes and required information:
#'
#' # "snapshot" mode
#' As with `[create_target()]`, a snapshot view is looking at a particular place,
#' time, and dose target, but with varying birth cohorts. That means the sum of
#' birth cohort and age is constant: if birth cohort 1 is age 10, then cohort 2 is 9,
#' and so on.
#'
#' Snapshots requires `obs_id`, `loc_id`, `dose`, `age_min`, and `cohort`
#' the reference cohort corresponding to the oldest age. `age_max` may be provided,
#' but if missing or `NA`, is assumed to be `age_min` + 1. `age_max` corresponds
#' to the first *excluded* age - i.e.
#'
#' $$
#' \textrm{age}\in\left[\textrm{age_min},\textrm{age_max}\right)
#' $$
#'
#' Taking this approach to `age_max` enables this method to naturally support partial
#' cohorts. For example, if `age_max = 18.5` and `age_min = 17`, then age 17 population
#' has 2/3rds the weight and the age 18 population has 1/3rd. `age_min` works the
#' same way.
#'
#' Note that "snapshot" mode assumes that all populations are uniformly sized with
#' respect to weighting. This assumption may be inadequate when population age
#' groups contributing to an observation are very differently sized.
#'
#' @return A `data.table` representing the populations mapping.
#' @autoglobal
#' @export
create_observation_populations <- function(
  observations,
  mode = "snapshot",
  ...
) {
  mode <- match.arg(mode)

  obs_dt <- canonicalize_observations(observations, drop_extra = FALSE)

  required_cols <- switch(
    mode,
    "snapshot" = c("loc_id", "cohort", "age_min", "dose")
  )

  # check that required columns are present in either observations or ...
  dot_args <- list(...)
  missing_cols <- setdiff(
    required_cols,
    c(names(observations), names(dot_args))
  )
  if (length(missing_cols) > 0) {
    stop(
      "The ",
      mode,
      " requires the following column(s): ",
      toString(required_cols),
      "; but the following are missing from the combination of ",
      "'observations' and '...': ",
      toString(missing_cols),
      call. = FALSE
    )
  }

  optional_cols <- c("age_max")

  dup_cols <- intersect(
    c(required_cols, optional_cols),
    intersect(names(observations), names(dot_args))
  )
  if (length(dup_cols) > 0) {
    stop(
      "The following column(s) are specified in both 'observations' and '...': ",
      toString(dup_cols),
      call. = FALSE
    )
  }

  # merge required columns into obs_dt
  if (length(dot_args) > 0) {
    obs_dt[, c(names(dot_args)) := dot_args]
  }

  if (mode == "snapshot") {
    # confirm: dose and cohort are positive integers
    assert_positive_integer(obs_dt, "dose")
    assert_positive_integer(obs_dt, "cohort")

    # confirm: age_min, age_max are in positive numerics, with age_min <= age_max
    assert_positive_numeric(obs_dt, "age_min")
    # age_max is optional; if completely missing or rows == NA, assumed to be age_min
    if (!("age_max") %in% names(obs_dt)) {
      obs_dt[, age_max := NA_integer_]
    }
    obs_dt[is.na(age_max), age_max := age_min + 1L]
    assert_positive_numeric(obs_dt, "age_max")

    if (obs_dt[, !all(age_min < age_max)]) {
      stop("age_min must be strictly less than age_max", call. = FALSE)
    }

    pop_dt <- obs_dt[,
      {
        age_span <- age_max - age_min
        a_min <- as.integer(age_min)
        a_max <- as.integer(ceiling(age_max - 1L))
        age_seq <- seq.int(a_min, a_max)

        contrib <- pmin(age_seq + 1, age_max) - pmax(age_seq, age_min)
        wts <- contrib / age_span

        .(
          loc_id = loc_id,
          cohort = as.integer(cohort + a_max - age_seq),
          age = age_seq,
          dose = dose,
          weight = wts
        )
      },
      by = obs_id
    ]

    col_order <- intersect(
      c("obs_id", "loc_id", "cohort", "age", "dose", "weight"),
      names(pop_dt)
    )
    data.table::setcolorder(pop_dt, col_order)

    pop_dt[]
  }
}

#' @keywords internal
gcd <- function(a, b) {
  while (b != 0) {
    temp <- b
    b <- a %% b
    a <- temp
  }
  a
}

#' @keywords internal
lcm <- function(a, b) {
  (a * b) / gcd(a, b)
}

#' @keywords internal
compute_recycled_target_len <- function(lens) {
  target_len <- lens[1]
  for (len in lens[-1L]) {
    target_len <- lcm(target_len, len)
  }
  target_len
}

#' @keywords internal
validate_vec_inputs <- function(location, age, cohort, dose) {
  if (missing(age) || missing(cohort) || missing(dose)) {
    stop(
      "age, cohort, and dose must be supplied when location is a vector",
      call. = FALSE
    )
  }

  na_args <- c("location", "age", "cohort", "dose")[which(
    c(
      any(is.na(location)),
      any(is.na(age)),
      any(is.na(cohort)),
      any(is.na(dose))
    )
  )]

  if (length(na_args) > 0) {
    stop(
      "No arguments may have NA values; the following do: ",
      toString(na_args),
      call. = FALSE
    )
  }

  n_loc <- length(location)
  n_age <- length(age)
  n_coh <- length(cohort)
  n_dos <- length(dose)

  zero_lens <- c("location", "age", "cohort", "dose")[which(
    c(n_loc, n_age, n_coh, n_dos) == 0L
  )]

  if (length(zero_lens) > 0) {
    stop(
      "No arguments may have length zero; the following do: ",
      toString(zero_lens),
      call. = FALSE
    )
  }
  c(n_loc = n_loc, n_age = n_age, n_coh = n_coh, n_dos = n_dos)
}

#' @title Construct a target grid for prediction
#'
#' @description
#' Builds a target grid, for use with `[predict.imugap_fit()]`, from vectors of
#' locations, ages, cohorts, and doses. This is pure construction and does not
#' reference a fitted model, so it can be called without a fit (e.g. to expand a
#' request into rows before any fit exists). To validate a target against a
#' specific fit -- or to canonicalize a target you built yourself as a
#' `data.frame` -- use `[canonicalize_target()]`; `[predict.imugap_fit()]` does
#' this for you.
#'
#' @param location a vector of location IDs to target.
#' @param age vector of ages for which to predict coverage, consistent with
#'   `[canonicalize_populations()]`.
#' @param cohort vector of cohorts for which to predict coverage, consistent with
#'   `[canonicalize_populations()]`.
#' @param dose vector of doses for which to predict coverage, consistent with
#'   `[canonicalize_observations()]`.
#' @param mode one of `"error"` (default), `"enumerate"`, `"recycle"`, or
#'   `"snapshot"`, controlling how the vector inputs combine:
#'
#'   - `"error"`: all vector inputs must have the same length.
#'   - `"enumerate"`: all combinations of the inputs.
#'   - `"recycle"`: recycle the inputs out to the least-common-multiple length.
#'   - `"snapshot"`: `cohort` must be a single reference value (the oldest
#'     cohort); locations, ages, and doses are enumerated with a cohort for each
#'     age such that `age + cohort` is constant, using the **maximum** value of
#'     `age` to set that constant (`cohort_i = cohort_ref + max(age) - age_i`),
#'     i.e. a snapshot in time.
#'
#' @return a `data.table` target grid with columns `obs_c_id`, `loc_id`, `age`,
#'   `cohort`, `dose`, and `weight`.
#'
#' @seealso `[canonicalize_target()]`, `[predict.imugap_fit()]`
#'
#' @examples
#' # "error" mode: all vector inputs must have the same length.
#' create_target(
#'   location = c("Blue Heron School", "Bluebird Learning Center"),
#'   age = c(1, 2), cohort = c(2, 3), dose = c(1, 1), mode = "error"
#' )
#'
#' # "enumerate": all combinations of the inputs.
#' create_target(
#'   location = c("Blue Heron School", "Bluebird Learning Center"),
#'   age = c(1, 2), cohort = c(2, 3), dose = c(1), mode = "enumerate"
#' )
#'
#' # "snapshot": cohort is a single reference; cohorts are set so age + cohort is
#' # constant, using max(age).
#' create_target(
#'   location = c("Blue Heron School", "Bluebird Learning Center"),
#'   age = c(1, 2, 3), cohort = 5, dose = c(1), mode = "snapshot"
#' )
#'
#' @importFrom data.table as.data.table copy data.table
#' @export
create_target <- function(
  location,
  age,
  cohort,
  dose,
  mode = c("error", "enumerate", "recycle", "snapshot")
) {
  mode <- match.arg(mode)
  lens <- validate_vec_inputs(location, age, cohort, dose)

  if (mode == "error") {
    if (length(unique(lens)) > 1L) {
      stop(
        "All arguments must have the same length in 'error' mode",
        call. = FALSE
      )
    }
    target <- data.table::data.table(
      loc_id = location,
      age = age,
      cohort = cohort,
      dose = dose,
      weight = 1.0
    )
  } else if (mode %in% c("enumerate", "snapshot")) {
    if (mode == "snapshot" && length(cohort) != 1L) {
      stop(
        "cohort must be a single reference value in 'snapshot' mode",
        call. = FALSE
      )
    }
    target <- data.table::as.data.table(expand.grid(
      loc_id = location,
      age = age,
      cohort = cohort,
      dose = dose,
      weight = 1.0,
      stringsAsFactors = FALSE
    ))

    if (mode == "snapshot") {
      ref_cohort <- cohort
      max_age <- max(age)
      target[, cohort := ref_cohort + max_age - age]
    }
  } else if (mode == "recycle") {
    target_len <- compute_recycled_target_len(lens)

    target <- data.table::data.table(
      loc_id = rep_len(location, target_len),
      age = rep_len(age, target_len),
      cohort = rep_len(cohort, target_len),
      dose = rep_len(dose, target_len),
      weight = 1.0
    )
  }
  target[, obs_c_id := seq_len(.N)]
  data.table::setcolorder(
    target,
    c("obs_c_id", "loc_id", "age", "cohort", "dose", "weight")
  )
  target[]
}
