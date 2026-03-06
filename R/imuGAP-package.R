#' The 'imuGAP' package.
#'
#' @description A package for estimating measles vaccine coverage
#'
#' @name imuGAP-package
#' @aliases imuGAP
#' @useDynLib imuGAP, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @importFrom rstan sampling
#' @importFrom rstantools rstan_config
#' @importFrom RcppParallel RcppParallelLibs
#'
#' @references
#' Stan Development Team (NA). RStan: the R interface to Stan.
#' R package version NA. https://mc-stan.org
#'
"_PACKAGE"

.onAttach <- function(libname, pkgname) {
  link <- Sys.which("imugap")
  if (!nzchar(link)) return()

  target <- Sys.readlink(link)
  if (!nzchar(target)) return()

  expected <- system.file("scripts", "imugap.R", package = pkgname)
  if (!nzchar(expected)) return()

  if (normalizePath(target, mustWork = FALSE) != normalizePath(expected, mustWork = FALSE)) {
    packageStartupMessage(
      "Note: 'imugap' on PATH points to a different install. ",
      "Run imuGAP::install_cli() to update."
    )
  }
}
