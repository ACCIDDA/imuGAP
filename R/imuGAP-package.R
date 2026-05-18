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

#' @title Example Stan Fit
#'
#' @description
#' A reference `stanfit` object produced by running [imuGAP()] on the bundled
#' `locations_sim`, `populations_sim`, and `observations_sim` datasets. Intended
#' as a lightweight fixture for examples, tests, and downstream tooling that
#' needs a real fit without paying the cost of recompiling or re-running the
#' Stan model.
#'
#' @details
#' Generated with the same minimal sampler settings as the smoke test:
#'  - `iter = 100`
#'  - `chains = 1`
#'  - `seed = 1L`
#'  - `refresh = 0`
#'
#' These settings are not enough for convergence; `fit_sim` is a wiring
#' fixture, not a scientifically meaningful posterior. The generating script
#' lives at `data-raw/fit_sim.R` and can be re-run from the package root with
#' `Rscript data-raw/fit_sim.R`.
#'
#' Note that `stanfit` objects bundle references to the compiled Stan model
#' and can be sensitive to major version changes in `rstan` and
#' `StanHeaders`. If a future install fails to load `fit_sim`, regenerate it
#' via `data-raw/fit_sim.R`.
#'
#' @format A `stanfit` object as returned by [rstan::sampling()].
"fit_sim"
