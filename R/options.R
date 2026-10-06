#' @title imuGAP Model Options
#'
#' @description
#' Configures model-side options for `imuGAP` estimation.
#'
#' @param df single positive integer; degrees of freedom to use for the cohort B-spline
#'   basis expansion (default: 5L).
#' @param dose_schedule ascending integer vector of ages at which each dose `1..n`
#'   becomes eligible (default: `c(1, 4)` for 2-dose vaccines).
#' @param compute_log_lik logical scalar; compute pointwise log-likelihood during
#'   sampling? (default: `FALSE`).
#' @param model character string specifying the model formulation (default: `"default"`).
#'   Options include `"default"` (or `"logit"`) for the logit link and `"probit"` for the
#'   probit link. Dispatch to optimized single versus multi-layer versions occurs
#'   automatically within `[sampling()]`.
#'
#' @examples
#' imugap_options()
#' imugap_options(dose_schedule = c(1, 3))
#' imugap_options(model = "probit")
#'
#' @return a named list, of `imuGAP` model options.
#' @export
imugap_options <- function(
  df = 5L,
  dose_schedule = c(1, 4),
  compute_log_lik = FALSE,
  model = c("default", "logit", "probit")
) {
  model <- match.arg(model)

  stop_fmt_if(length(df) != 1L, ERR_OPT_DF_SINGLE)
  df <- assert_positive_int(df, "df")

  dose_schedule <- assert_positive_int(dose_schedule, "dose_schedule")
  stop_fmt_if(
    is.unsorted(dose_schedule, strictly = TRUE),
    ERR_OPT_DOSE_SCHEDULE
  )

  stop_fmt_if(
    !is.logical(compute_log_lik) ||
      length(compute_log_lik) != 1L ||
      is.na(compute_log_lik),
    ERR_OPT_COMPUTE_LOG_LIK
  )

  list(
    df = df,
    dose_schedule = dose_schedule,
    compute_log_lik = compute_log_lik,
    model = model
  )
}
