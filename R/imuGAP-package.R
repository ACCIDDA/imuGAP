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

## usethis namespace: start
#' @importFrom data.table data.table
#' @importFrom data.table :=
#' @importFrom data.table .SD
#' @importFrom data.table .BY
#' @importFrom data.table .N
#' @importFrom data.table .I
#' @importFrom data.table .GRP
#' @importFrom data.table .NGRP
#' @importFrom data.table .EACHI
## usethis namespace: end
NULL

.onAttach <- function(libname, pkgname) {
  link <- Sys.which("imugap")
  if (!nzchar(link)) return()

  target <- Sys.readlink(link)
  if (!nzchar(target)) return()

  expected <- system.file("scripts", "imugap.R", package = pkgname)
  if (!nzchar(expected)) return()

  resolved <- normalizePath(file.path(dirname(link), target), mustWork = FALSE)
  if (resolved != normalizePath(expected, mustWork = FALSE)) {
    packageStartupMessage(
      "Note: 'imugap' on PATH points to a different install. ",
      "Run imuGAP::install_cli() to update."
    )
  }
}
