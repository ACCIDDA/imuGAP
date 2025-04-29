
#' @title Immunity: Geographic & Age-based Projection, `imgap`
#'
#' @description This function estimates current coverage, by age and location.
#'
#' @param x Numeric vector of input values.
#' @param y Numeric vector of output values.
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
#' @export
imgap <- function(
  x, y, model = c("ar_cnty", "ar_schl", "cnty_noisy", "cnty_schl_static"),
  ...
) {
  model <- match.arg(model)
  standata <- list(x = x, y = y, N = length(y))
  out <- rstan::sampling(stanmodels[[model]], data = standata, ...)
  return(out)
}