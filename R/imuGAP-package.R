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

#' @title Example Location Data
#'
#' @description
#' A dataset providing example location input.
#'
#' @format A `[data.table()]` with 28 rows and 2 columns:
#'  - `loc_id`, a string, the location
#'  - `parent_id`, a string, the location parents
"locations_sim"

#' @title Example Population Data
#'
#' @description A dataset containing the meta-data about vaccine coverage observations.
#'
#' @format A `[data.table()]` with 750 rows and 6 columns:
#'  - `obs_id`, a number, the observation (foreign key to observations)
#'  - `loc_id`, a string, the location (foreign key to locations)
#'  - `cohort`, a number, the birth cohort
#'  - `age`, a number, the age of cohort at time of observation
#'  - `dose`, a number, which dose the observation concerns
#'  - `weight`, a number (0-1), fraction of the observation this row represents
"populations_sim"

#' @title Example Observation Data
#'
#' @description A dataset containing vaccine coverage observations.
#'
#' @format A `[data.table()]` with 698 rows and 4+ columns:
#'  - `obs_id`, a number, the observation id (primary key)
#'  - `positive`, a number, how many individuals had the vaccine
#'  - `sample_n`, a number, the number of individuals in the observations
#'  - `censored`, a number, 1 if `positive` is right censored and
#'                `NA` if uncensored
#'  - ... assorted other columns that are irrelevant to use as observations arg
"observations_sim"

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
