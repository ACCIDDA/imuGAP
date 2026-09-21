# ==============================================================================
# Centralized Error and Warning Messages, Formatting, and Signaling Infrastructure
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Message Formatting and Signaling Functions
# ------------------------------------------------------------------------------

#' @title Format a message string with named placeholders or sprintf specifiers
#'
#' @description
#' Formats a template string. If named arguments are provided and `{name}`
#' placeholders exist in `fmt`, `{name}` tokens are replaced with matching argument values.
#' Otherwise, arguments are forwarded to [sprintf()].
#'
#' @param fmt character template string.
#' @param ... values to interpolate.
#' @param width integer maximum width for vector formatting via `toString()` (default: `80L`).
#'
#' @return a character scalar, the formatted message.
#'
#' @keywords internal
#' @noRd
format_message <- function(fmt, ..., width = 80L) {
  args <- list(...)
  if (length(args) == 0L) {
    return(fmt)
  }
  arg_names <- names(args)
  if (!is.null(arg_names) && any(nzchar(arg_names))) {
    for (i in seq_along(args)) {
      nm <- arg_names[i]
      if (nzchar(nm)) {
        fmt <- gsub(
          paste0("{", nm, "}"),
          toString(args[[i]], width = width),
          fmt,
          fixed = TRUE
        )
      }
    }
    fmt
  } else {
    sprintf(fmt, ...)
  }
}

#' @title Signal an error if a condition is met with formatted message
#'
#' @description
#' Evaluates `cond` and if `TRUE`, raises an error formatted with `format_message()`.
#'
#' @param cond logical expression to evaluate.
#' @param fmt character format string.
#' @param ... additional arguments passed to `format_message()`.
#' @param n frame offset integer specifying call stack depth for call attribution
#'   (default: `1L`). If `n <= 0L`, call attribution is suppressed (`NULL`).
#'
#' @keywords internal
#' @noRd
stop_fmt_if <- function(cond, fmt, ..., n = 1L) {
  if (isTRUE(cond)) {
    call_obj <- if (n > 0L) sys.call(-n) else NULL
    stop(simpleError(format_message(fmt, ...), call = call_obj))
  }
}

#' @title Signal a warning if a condition is met with formatted message
#'
#' @description
#' Evaluates `cond` and if `TRUE`, signals a warning formatted with `format_message()`.
#'
#' @param cond logical expression to evaluate.
#' @param fmt character format string.
#' @param ... additional arguments passed to `format_message()`.
#' @param n frame offset integer specifying call stack depth for call attribution
#'   (default: `1L`). If `n <= 0L`, call attribution is suppressed (`NULL`).
#'
#' @return a logical scalar, indicating whether `cond` evaluated to `TRUE`.
#'
#' @keywords internal
#' @noRd
warn_fmt_if <- function(cond, fmt, ..., n = 1L) {
  cond_val <- isTRUE(cond)
  if (cond_val) {
    call_obj <- if (n > 0L) sys.call(-n) else NULL
    warning(simpleWarning(format_message(fmt, ...), call = call_obj))
  }
  cond_val
}

# ------------------------------------------------------------------------------
# 2. Checkers & Assertion Error/Warning Templates
# ------------------------------------------------------------------------------

#' @title Integer Column Assertion Error
#' @description Raised when a column contains non-integer values.
#' @param {name} table name or expression.
#' @param {col} column name.
#' @keywords internal
#' @noRd
ERR_MUST_BE_INTEGER <- "`{name}` column '{col}' must contain integers"

#' @title Missing NA Assertion Error
#' @description Raised when a column contains NA values where disallowed.
#' @param {name} table name or expression.
#' @param {col} column name.
#' @keywords internal
#' @noRd
ERR_CANNOT_HAVE_NA <- "`{name}` column '{col}' cannot contain NA values"

#' @title Positive Value Assertion Error
#' @description Raised when a column contains values not strictly greater than 0.
#' @param {name} table name or expression.
#' @param {col} column name.
#' @keywords internal
#' @noRd
ERR_MUST_BE_GT_ZERO <- "`{name}` column '{col}' must contain values > 0"

#' @title Non-Negative Value Assertion Error
#' @description Raised when a column contains values strictly less than 0.
#' @param {name} table name or expression.
#' @param {col} column name.
#' @keywords internal
#' @noRd
ERR_MUST_BE_GTE_ZERO <- "`{name}` column '{col}' must contain values >= 0"

#' @title Upper Bound Integer Assertion Error
#' @description Raised when column values exceed an upper bound limit.
#' @param {name} table name or expression.
#' @param {col} column name.
#' @param {max_val} maximum allowed integer value.
#' @keywords internal
#' @noRd
ERR_MUST_BE_LTE_MAX <- "`{name}` column '{col}' must contain values <= {max_val}"

#' @title Missing Required Set Value Error
#' @description Raised when column does not contain all required set values.
#' @param {name} table name or expression.
#' @param {col} column name.
#' @keywords internal
#' @noRd
ERR_SET_EQUIV_MISSING <- "`{name}` column '{col}' is missing required set values"

#' @title Permitted Set Value Violation Error
#' @description Raised when column contains values outside the permitted set.
#' @param {name} table name or expression.
#' @param {col} column name.
#' @keywords internal
#' @noRd
ERR_SET_EQUIV_EXTRA <- "`{name}` column '{col}' contains values outside permitted set"

#' @title Parent Set Subset Violation Error
#' @description Raised when column values are not contained in the parent set.
#' @param {name} table name or expression.
#' @param {col} column name.
#' @param {missing} string representation of missing elements.
#' @keywords internal
#' @noRd
ERR_SUBSET_MISSING <- "`{name}` column '{col}' contains values not in parent set: missing {missing}"

#' @title Missing Required Table Columns Error
#' @description Raised when required columns are absent from a table.
#' @param {name} table name or expression.
#' @param {missing} string list of missing column names.
#' @keywords internal
#' @noRd
ERR_MISSING_COLS <- "`{name}` is missing required column(s): {missing}"

#' @title Numeric Column Assertion Error
#' @description Raised when a column is not numeric.
#' @param {name} table name or expression.
#' @param {col} column name.
#' @keywords internal
#' @noRd
ERR_MUST_BE_NUMERIC <- "`{name}` column '{col}' must contain numeric values"

#' @title Unexpected Extra Columns Warning
#' @description Warned when unexpected extra columns are present in a table.
#' @param {name} table name or expression.
#' @param {extra} string list of extra column names.
#' @keywords internal
#' @noRd
MSG_EXTRA_COLS <- "`{name}` contains unexpected extra column(s): {extra}"

#' @title Numeric Argument Assertion Error
#' @description Raised when an argument is not numeric.
#' @param {name} argument name.
#' @keywords internal
#' @noRd
ERR_ARG_MUST_BE_NUMERIC <- "'{name}' must be numeric"

#' @title Argument Minimum Length Assertion Error
#' @description Raised when an argument has zero length.
#' @param {name} argument name.
#' @keywords internal
#' @noRd
ERR_ARG_MIN_LENGTH <- "`length({name})` must be >= 1"

#' @title Argument NA Assertion Error
#' @description Raised when an argument contains NA values.
#' @param {name} argument name.
#' @keywords internal
#' @noRd
ERR_ARG_CANNOT_HAVE_NA <- "'{name}' may not contain NAs"

#' @title Argument Integer Assertion Error
#' @description Raised when an argument contains non-integer numbers.
#' @param {name} argument name.
#' @keywords internal
#' @noRd
ERR_ARG_MUST_BE_INTEGER <- "'{name}' must be integers"

#' @title Argument Positive Value Assertion Error
#' @description Raised when an argument contains non-positive values.
#' @param {name} argument name.
#' @keywords internal
#' @noRd
ERR_ARG_MUST_BE_GT_ZERO <- "'{name}' must be positive"

# ------------------------------------------------------------------------------
# 3. Location Hierarchy Templates
# ------------------------------------------------------------------------------

#' @title Duplicate Location ID Error
#' @description Raised when location identifiers are not unique.
#' @param {n_duplicates} count of duplicate IDs.
#' @param {duplicates} string list of duplicate location IDs.
#' @keywords internal
#' @noRd
ERR_LOCATIONS_UNIQUE_IDS <- paste0(
  "`locations` column 'loc_id' must contain unique values; ",
  "found {n_duplicates} duplicate(s): {duplicates}; ",
  "use `subset(locations, duplicated(loc_id) | duplicated(loc_id, fromLast = TRUE))` ",
  "to resolve invalid entries"
)

#' @title Single Root Location Hierarchy Error
#' @description Raised when location hierarchy does not have exactly one root.
#' @param {n_roots} number of candidate root locations found.
#' @param {details} details string with candidate root names if any.
#' @keywords internal
#' @noRd
ERR_LOCATIONS_SINGLE_ROOT <- paste0(
  "`locations` must have exactly one root location; found {n_roots}{details}; ",
  "use `subset(locations, is.na(parent_id))` to inspect candidate root entries"
)

#' @title Root Details Sub-string
#' @description Details suffix for multiple root locations in `locations`.
#' @param {roots} string list of candidate root locations.
#' @keywords internal
#' @noRd
ERR_LOCATIONS_ROOT_DETAILS <- ": {roots}"

#' @title Location Hierarchy Cycle Error
#' @description Raised when cycles are detected in the location hierarchy.
#' @param {n_locations} number of unassigned locations in cycle(s).
#' @param {locations} string list of unassigned location IDs.
#' @keywords internal
#' @noRd
ERR_LOCATIONS_NO_CYCLES <- paste0(
  "`locations` hierarchy cannot contain cycles; found {n_locations} location(s) ",
  "in cycle(s): {locations}"
)

#' @title Single Child Chain Invariant Error
#' @description Raised when a location node has exactly 1 offspring.
#' @param {n_locations} number of parent locations with 1 offspring.
#' @param {locations} string list of parent location IDs.
#' @keywords internal
#' @noRd
ERR_LOCATIONS_OFFSPRING_COUNT <- paste0(
  "`locations` each location must have either 0 or strictly greater than 1 offspring; ",
  "found {n_locations} location(s) with exactly 1 offspring: {locations}; ",
  "use `subset(locations, parent_id %in% c({locations}))` to resolve single-child chains"
)

# ------------------------------------------------------------------------------
# 4. Observation Data Templates
# ------------------------------------------------------------------------------

#' @title Observation ID NA Error
#' @description Raised when `obs_id` column contains NA values.
#' @param {n_nas} count of NA values.
#' @param {rows} string list of row indices containing NA.
#' @keywords internal
#' @noRd
ERR_OBS_NA_ID <- paste0(
  "`observations` column 'obs_id' cannot contain NA values; ",
  "found {n_nas} NA value(s) at row(s): {rows}; ",
  "use `subset(observations, is.na(obs_id))` to resolve invalid entries"
)

#' @title Duplicate Observation ID Error
#' @description Raised when `obs_id` values are not unique.
#' @param {n_duplicates} count of duplicate IDs.
#' @param {duplicates} string list of duplicate observation IDs.
#' @keywords internal
#' @noRd
ERR_OBS_DUP_ID <- paste0(
  "`observations` column 'obs_id' must contain unique values; ",
  "found {n_duplicates} duplicate(s): {duplicates}; ",
  "use `subset(observations, duplicated(obs_id) | duplicated(obs_id, fromLast = TRUE))` ",
  "to resolve invalid entries"
)

#' @title Observation Positive Greater Than Sample Size Error
#' @description Raised when `positive` count exceeds `sample_n`.
#' @param {n_rows} number of invalid rows.
#' @param {obs_ids} string list of invalid observation IDs.
#' @keywords internal
#' @noRd
ERR_OBS_POS_GT_SAMPLE <- paste0(
  "`observations` column 'positive' must be <= 'sample_n'; ",
  "found {n_rows} invalid row(s) with obs_id: {obs_ids}; ",
  "use `subset(observations, positive > sample_n)` to resolve invalid entries"
)

#' @title Censored Column Numeric Error
#' @description Raised when `censored` column is non-numeric.
#' @keywords internal
#' @noRd
ERR_OBS_CENSORED_NUMERIC <- "`observations` column 'censored' must contain numeric values"

#' @title Censored Column Values Error
#' @description Raised when `censored` column contains values other than NA or 1.
#' @keywords internal
#' @noRd
ERR_OBS_CENSORED_VALUES <- paste0(
  "`observations` column 'censored' must contain NA (uncensored) ",
  "or 1 (right-censored); ",
  "use `subset(observations, !is.na(censored) & censored != 1)` to resolve invalid entries"
)

# ------------------------------------------------------------------------------
# 5. Populations & Dose Schedule Validation Templates
# ------------------------------------------------------------------------------

#' @title Missing Weight Column Error
#' @description Raised when `populations` lacks `weight` and has duplicate `obs_id`.
#' @keywords internal
#' @noRd
ERR_POP_MISSING_WEIGHT_COL <- paste0(
  "`populations` is missing required column(s): 'weight' ",
  "(required when observations contain multiple sub-groups); ",
  "use `subset(populations, duplicated(obs_id))` to inspect duplicate observation entries"
)

#' @title Population Weights Sum Error
#' @description Raised when weights do not sum to 1 by `obs_id`.
#' @keywords internal
#' @noRd
ERR_POP_WEIGHT_SUM <- "`populations` column 'weight' must sum to 1 by 'obs_id'"

#' @title Maximum Layer Observation Requirement Error
#' @description Raised when no observations exist at the deepest location layer.
#' @param {max_depth} integer depth of the deepest location layer.
#' @keywords internal
#' @noRd
ERR_POP_MAX_LAYER_OBS <- paste0(
  "`populations` must contain at least one observation at the ",
  "maximum location layer depth ({max_depth})"
)

#' @title Dose Exceeds Schedule Limit Error
#' @description Raised when `populations` contains dose values exceeding `dose_schedule` length.
#' @param {n_doses} length of dose schedule / maximum configured doses.
#' @param {max_dose} maximum dose value found in `populations`.
#' @keywords internal
#' @noRd
ERR_POP_DOSE_EXCEEDS_SCHED <- paste0(
  "maximum dose is {n_doses} (`dose_schedule` length == {n_doses}), but `populations` contains ",
  "dose(s) exceeding this limit (found max dose {max_dose}); ",
  "use `subset(populations, dose > {n_doses})` or configure ",
  "`imugap_options(dose_schedule = ...)` to resolve invalid entries"
)

#' @title Dose Age Incompatibility Error
#' @description Raised when observations for a dose have all ages <= dose changepoint age.
#' @param {dose} dose number.
#' @param {sched_age} eligibility age for the dose in `dose_schedule`.
#' @keywords internal
#' @noRd
ERR_POP_DOSE_INCOMPATIBLE <- paste0(
  "dose {dose} requires age > {sched_age} (`dose_schedule[{dose}] == {sched_age}`), but ",
  "`populations` contains observations where all ages are <= {sched_age}; ",
  "use `subset(populations, dose == {dose} & age <= {sched_age})` or configure ",
  "`imugap_options(dose_schedule = ...)` to resolve invalid entries"
)

#' @title Final Dose Unobserved Error
#' @description Raised when final dose in `dose_schedule` is unobserved in `populations`.
#' @param {n_doses} final dose number.
#' @keywords internal
#' @noRd
ERR_DOSE_FINAL_NOT_OBSERVED <- paste0(
  "maximum dose ({n_doses}) must be observed in `populations`; ",
  "configure `imugap_options(dose_schedule = ...)` to match observed doses"
)

# ------------------------------------------------------------------------------
# 6. Target Construction & Canonicalization Templates
# ------------------------------------------------------------------------------

#' @title Non-unique Target Weights Error
#' @description Raised when target specifies non-unique `obs_id` with weights (not yet supported).
#' @keywords internal
#' @noRd
ERR_TARGET_NON_UNIQUE_WEIGHTS <- paste0(
  "`target` non-unique observation IDs with weights are not yet ",
  "supported (see https://github.com/ACCIDDA/imuGAP/issues/79)"
)

#' @title Target Sequential Canonical ID Error
#' @description Raised when target `obs_c_id` does not match 1:nrow(target).
#' @keywords internal
#' @noRd
ERR_TARGET_INVALID_OBS_C_ID <- "`target` column 'obs_c_id' must equal 1:nrow(target)"

#' @title Target Observation ID Uniqueness Error
#' @description Raised when target `obs_id` contains duplicates or NAs.
#' @keywords internal
#' @noRd
ERR_TARGET_INVALID_OBS_ID <- "`target` column 'obs_id' must contain unique non-NA values"

#' @title Target Weight Value Error
#' @description Raised when target `weight` contains values not equal to 1.
#' @keywords internal
#' @noRd
ERR_TARGET_INVALID_WEIGHT <- "`target` column 'weight' must equal 1"

#' @title Target Unknown Location Error
#' @description Raised when target `loc_id` values do not exist in the fitted model locations.
#' @param {invalid_locs} string list of unknown location IDs.
#' @keywords internal
#' @noRd
ERR_TARGET_INVALID_LOCS <- paste0(
  "`target` column 'loc_id' must all exist in `fit$locations`; ",
  "invalid location(s): {invalid_locs}; ",
  "use `subset(target, !(loc_id %in% fit$locations$loc_id))` to resolve invalid entries"
)

#' @title Target Invalid Dose Value Error
#' @description Raised when target `dose` values fall outside 1..n_doses.
#' @param {n_doses} maximum dose in fitted model.
#' @param {rows} string list of invalid row indices.
#' @keywords internal
#' @noRd
ERR_TARGET_INVALID_DOSE <- paste0(
  "`target` column 'dose' must contain values between 1 and ",
  "fit$data$n_doses ({n_doses}); invalid row(s): {rows}; ",
  "use `subset(target, dose < 1 | dose > {n_doses})` to resolve invalid entries"
)

#' @title Target Invalid Age Value Error
#' @description Raised when target `age` values fall outside 1..n_yr.
#' @param {n_yr} maximum age in fitted model.
#' @param {rows} string list of invalid row indices.
#' @keywords internal
#' @noRd
ERR_TARGET_INVALID_AGE <- paste0(
  "`target` column 'age' must contain values between 1 and ",
  "fit$data$n_yr ({n_yr}); invalid row(s): {rows}; ",
  "use `subset(target, age < 1 | age > {n_yr})` to resolve invalid entries"
)

#' @title Target Invalid Cohort Value Error
#' @description Raised when target `cohort` values fall outside 1..n_cohort.
#' @param {n_cohort} maximum cohort in fitted model.
#' @param {rows} string list of invalid row indices.
#' @keywords internal
#' @noRd
ERR_TARGET_INVALID_COHORT <- paste0(
  "`target` column 'cohort' must contain values between 1 and ",
  "fit$data$n_cohort ({n_cohort}); invalid row(s): {rows}; ",
  "use `subset(target, cohort < 1 | cohort > {n_cohort})` to resolve invalid entries"
)

# ------------------------------------------------------------------------------
# 7. Helper Function Templates (create_observation_populations, create_target)
# ------------------------------------------------------------------------------

#' @title Missing Mode Columns Error
#' @description Raised when required columns for helper mode are missing.
#' @param {mode} mode name.
#' @param {required} string list of required column names.
#' @param {missing} string list of missing column names.
#' @keywords internal
#' @noRd
ERR_HELP_MODE_MISSING_COLS <- paste0(
  "mode '{mode}' requires column(s): {required}; missing from combination of ",
  "`observations` and '...': {missing}"
)

#' @title Duplicate Mode Columns Error
#' @description Raised when columns are specified in both `observations` and `...`.
#' @param {cols} string list of duplicated column names.
#' @keywords internal
#' @noRd
ERR_HELP_MODE_DUP_COLS <- paste0(
  "the following column(s) are specified in both `observations` ",
  "and '...': {cols}"
)

#' @title Age Range Min-Max Error
#' @description Raised when `age_min` is not strictly less than `age_max`.
#' @keywords internal
#' @noRd
ERR_HELP_AGE_MIN_MAX <- paste0(
  "`age_min` must be strictly less than `age_max`; ",
  "use `subset(observations, age_min >= age_max)` to resolve invalid entries"
)

#' @title Missing Vector Inputs Error
#' @description Raised when vector inputs are incomplete for target creation.
#' @keywords internal
#' @noRd
ERR_HELP_VEC_INPUTS_MISSING <- paste0(
  "`age`, `cohort`, and `dose` must be supplied when ",
  "`location` is a vector"
)

#' @title Vector Inputs NA Error
#' @description Raised when vector arguments contain NA values.
#' @param {args} string list of arguments containing NA.
#' @keywords internal
#' @noRd
ERR_HELP_VEC_INPUTS_NA <- "arguments cannot contain NA values; the following do: {args}"

#' @title Vector Inputs Zero Length Error
#' @description Raised when vector arguments have length zero.
#' @param {args} string list of arguments with length zero.
#' @keywords internal
#' @noRd
ERR_HELP_VEC_INPUTS_ZERO_LEN <- "arguments cannot have length zero; the following do: {args}"

#' @title Error Mode Length Mismatch Error
#' @description Raised when vector arguments differ in length in 'error' mode.
#' @keywords internal
#' @noRd
ERR_HELP_ERROR_MODE_LEN <- "all arguments must have the same length in 'error' mode"

#' @title Snapshot Mode Single Cohort Error
#' @description Raised when `cohort` is not a single value in 'snapshot' mode.
#' @keywords internal
#' @noRd
ERR_HELP_SNAP_COHORT_SINGLE <- "`cohort` must be a single reference value in 'snapshot' mode"

# ------------------------------------------------------------------------------
# 8. Options, Stan, and Model Configuration Templates
# ------------------------------------------------------------------------------

#' @title Degrees of Freedom Option Error
#' @description Raised when `df` is not a single positive integer.
#' @keywords internal
#' @noRd
ERR_OPT_DF_SINGLE <- "`df` must be a single positive integer"

#' @title Dose Schedule Option Error
#' @description Raised when `dose_schedule` is not an ascending vector of positive integers.
#' @keywords internal
#' @noRd
ERR_OPT_DOSE_SCHEDULE <- "`dose_schedule` must be an ascending vector of positive integers"

#' @title Unknown Model Option Error
#' @description Raised when an unrecognized model formulation is requested.
#' @param {model} requested model name.
#' @keywords internal
#' @noRd
ERR_OPT_UNKNOWN_MODEL <- "`imugap_opts` unknown model '{model}'"

#' @title Invalid Stan Options Class Error
#' @description Raised when `stan_opts` was not generated by [stan_options()].
#' @keywords internal
#' @noRd
ERR_STAN_OPTS_CLASS <- "`stan_opts` must be created by stan_options()"

# ------------------------------------------------------------------------------
# 9. Fitting, Prediction, S3 Methods, & Interval Schedule Templates
# ------------------------------------------------------------------------------

#' @title Invalid imugap_fit Class Error
#' @description Raised when an object is not of class `imugap_fit`.
#' @param {name} variable or argument name.
#' @keywords internal
#' @noRd
ERR_NOT_IMUGAP_FIT <- "`{name}` must be an object of class 'imugap_fit'"

#' @title Invalid imugap_predict Class Error
#' @description Raised when an object is not of class `imugap_predict`.
#' @param {name} variable or argument name.
#' @keywords internal
#' @noRd
ERR_NOT_IMUGAP_PREDICT <- "`{name}` must be an object of class 'imugap_predict'"

#' @title Posterior Size Single Value Error
#' @description Raised when `posterior_size` has length != 1.
#' @keywords internal
#' @noRd
ERR_POSTERIOR_SIZE_SINGLE <- "`posterior_size` must be a single value"

#' @title Posterior Size Exceeds Draws Error
#' @description Raised when `posterior_size` exceeds total available posterior draws.
#' @param {posterior_size} requested draw count.
#' @param {n_draws} available draw count in fit.
#' @keywords internal
#' @noRd
ERR_POSTERIOR_SIZE_EXCEEDS <- paste0(
  "`posterior_size` ({posterior_size}) exceeds the {n_draws} available posterior ",
  "draws in `object`"
)

#' @title Subset Non-Logical Error
#' @description Raised when `subset` expression does not evaluate to logical.
#' @keywords internal
#' @noRd
ERR_SUBSET_NOT_LOGICAL <- "`subset` must be a logical vector"

#' @title Posterior Size Chain Multiple Warning
#' @description Warned when `posterior_size` is rounded up to a multiple of chain count.
#' @param {posterior_size} requested draw count.
#' @param {n_chains} number of MCMC chains.
#' @param {adjusted_size} rounded draw count used.
#' @keywords internal
#' @noRd
MSG_POSTERIOR_SIZE_ROUNDED <- paste0(
  "`posterior_size` ({posterior_size}) is not a multiple of the {n_chains} chains; ",
  "using {adjusted_size} draws instead"
)

#' @title Subsample Posterior Adequacy Warning
#' @description Warned when predicting on a sub-sample of posterior draws.
#' @param {posterior_size} number of sub-sampled draws used.
#' @keywords internal
#' @noRd
MSG_POSTERIOR_SUBSAMPLE_WARN <- paste0(
  "predict() is using a sub-sample of {posterior_size} posterior draws and does ",
  "not check whether it is adequate (chain mixing, effective sample ",
  "size); verify sufficiency statistics yourself"
)

#' @title Empty Dose Schedule Error
#' @description Raised when `dose_schedule` vector has length 0.
#' @keywords internal
#' @noRd
ERR_DOSE_SCHEDULE_EMPTY <- "`dose_schedule` must not be empty"

#' @title Dose Schedule Out of Bounds Error
#' @description Raised when `dose_schedule` contains no changepoints within 1..max_age.
#' @param {max_age} maximum observed age.
#' @keywords internal
#' @noRd
ERR_DOSE_SCHED_OOB <- "`dose_schedule` contains no changepoints within 1..{max_age}"

#' @title Empty Ages Vector Error
#' @description Raised when `ages` vector is empty or all NA.
#' @keywords internal
#' @noRd
ERR_AGES_EMPTY <- "`ages` must not be empty"

#' @title Ages Out of Bounds Error
#' @description Raised when `ages` contains no valid ages within 1..max_age.
#' @param {max_age} maximum observed age.
#' @keywords internal
#' @noRd
ERR_AGES_OOB <- "`ages` contains no valid ages within 1..{max_age}"
