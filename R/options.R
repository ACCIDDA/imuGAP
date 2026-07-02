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
