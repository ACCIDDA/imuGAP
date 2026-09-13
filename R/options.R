# Internal error message format strings for options.R
ERR_OPT_DF_SINGLE <- "`df` must be a single positive integer"
ERR_OPT_DOSE_SCHEDULE <- "`dose_schedule` must be an ascending vector of positive integers"
ERR_OPT_UNKNOWN_MODEL <- "`imugap_opts` unknown model '%s'"

#' @title imuGAP Model Options
#'
#' @description
#' Configures model-side options for `imuGAP` estimation.
#'
#' @param df single positive integer; degrees of freedom to use for the cohort B-spline
#'   basis expansion (default: 5L).
#' @param dose_schedule an ascending integer vector of ages at which each dose `1..n`
#'   becomes eligible (default: `c(1, 4)` for 2-dose vaccines).
#' @param model character string specifying the model formulation. Defaults to `"default"`,
#'   with dispatch to optimized single versus multilayer versions within `[sampling()]`
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
  model = c("default")
) {
  model <- match.arg(model)

  stop_fmt_if(length(df) != 1L, ERR_OPT_DF_SINGLE)
  df <- assert_positive_int(df, "df")

  dose_schedule <- assert_positive_int(dose_schedule, "dose_schedule")
  stop_fmt_if(
    is.unsorted(dose_schedule, strictly = TRUE),
    ERR_OPT_DOSE_SCHEDULE
  )

  list(
    df = df,
    dose_schedule = dose_schedule,
    model = model
  )
}
