# Validate that a value is a vector of positive integers, coercing to integer.
# Small generic helper used by imugap_options() and the print methods; it was
# previously provided by the vendored flexstanr backend file, but flexstanr does
# not export it, so imuGAP keeps its own copy after the flexstanr migration (#122).
assert_positive_int <- function(val, name) {
  if (!is.numeric(val)) {
    stop(sprintf("'%s' must be numeric", name), call. = FALSE)
  }
  if (length(val) < 1L) {
    stop(sprintf("length('%s') must be >= 1", name), call. = FALSE)
  }
  if (any(is.na(val))) {
    stop(sprintf("'%s' may not contain NAs", name), call. = FALSE)
  }
  if (any(val != as.integer(val))) {
    stop(sprintf("'%s' must be integers", name), call. = FALSE)
  }
  if (any(val < 1L)) {
    stop(sprintf("'%s' must be positive", name), call. = FALSE)
  }
  as.integer(val)
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
#' @examples
#' imugap_options()
#' imugap_options(dose_schedule = c(1, 3))
#'
#' @return a list of imuGAP model options
#' @export
imugap_options <- function(
  df = 5L,
  dose_schedule = c(1, 4),
  object = c("default")
) {
  if (length(df) != 1L) {
    stop("'df' must be a single positive integer", call. = FALSE)
  }
  df <- assert_positive_int(df, "df")

  dose_schedule <- assert_positive_int(dose_schedule, "dose_schedule")
  if (is.unsorted(dose_schedule, strictly = TRUE)) {
    stop(
      "'dose_schedule' must be an ascending vector of positive integers",
      call. = FALSE
    )
  }

  object <- switch(
    object,
    "default" = stanmodels$impute_school_coverage_process_v6,
    stop("Unknown model object: ", object)
  )
  as.list(environment())
}
