# Internal error message format strings for options.R
ERR_OPT_DF_SINGLE <- "`df` must be a single positive integer"
ERR_OPT_DOSE_SCHEDULE <- "`dose_schedule` must be an ascending vector of positive integers"
ERR_OPT_UNKNOWN_MODEL <- "`imugap_opts` unknown model '%s'"
ERR_OPT_SIGMA_LAYER_SCALE <- "`sigma_layer_scale` must be a single positive number"

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
#' @param sigma_layer_scale single positive numeric; scale parameter for the Cauchy prior
#'   on layer standard deviations `sigma_layer` (default: 2.5).
#'
#' @examples
#' imugap_options()
#' imugap_options(dose_schedule = c(1, 3))
#' imugap_options(sigma_layer_scale = 1.0)
#'
#' @return a list of imuGAP model options
#' @export
imugap_options <- function(
  df = 5L,
  dose_schedule = c(1, 4),
  model = c("default"),
  sigma_layer_scale = 2.5
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
    !is.numeric(sigma_layer_scale) ||
      length(sigma_layer_scale) != 1L ||
      is.na(sigma_layer_scale) ||
      sigma_layer_scale <= 0,
    ERR_OPT_SIGMA_LAYER_SCALE
  )

  list(
    df = df,
    dose_schedule = dose_schedule,
    model = model,
    sigma_layer_scale = as.numeric(sigma_layer_scale)
  )
}
