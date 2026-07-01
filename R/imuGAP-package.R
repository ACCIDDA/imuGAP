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
#' fixture, not a scientifically meaningful posterior. It is not tracked in
#' git: it is regenerated on build by `data-raw/fit_data.R` (run `just data-fit`
#' locally, or `just data` for the full pipeline).
#'
#' Note that `stanfit` objects bundle references to the compiled Stan model
#' and can be sensitive to major version changes in `rstan` and
#' `StanHeaders`. If a future install fails to load `fit_sim`, regenerate it
#' via `data-raw/fit_data.R`.
#'
#' @format A `stanfit` object as returned by [rstan::sampling()].
"fit_sim"

#' @title Example Latent Parameter Values
#'
#' @description
#' A list containing the true/latent parameter values used to simulate the
#' example datasets (`locations_sim`, `populations_sim`, `observations_sim`).
#'
#' @format A list with 8 components:
#'  - `phi_state`, a numeric vector of length 30 representing the state-specific
#'    baseline vaccine uptake propensity over cohorts.
#'  - `lambda`, a numeric vector of length 2 representing the rate parameters
#'    for vaccine doses 1 and 2 respectively.
#'  - `sigma_sch`, a number, the standard deviation of school-level random effects.
#'  - `sigma_cnty`, a number, the standard deviation of county-level random effects.
#'  - `off_sch`, a numeric vector of length 24 containing school-level random offsets.
#'  - `off_cnty`, a numeric vector of length 3 containing county-level random offsets.
#'  - `censor_reduction`, a number representing the censoring offset
#'    multiplier applied to censored observations (0.95).
#'  - `coverage`, a numeric vector of the true/background coverage for each row
#'    of `target_sim`, computed from the latent parameters above.
"latent_params_sim"

#' @title Example Prediction Target Populations
#'
#' @description
#' A dataset specifying the target populations for coverage prediction, generated
#' by calling [create_target()] on the simulated fit object `fit_sim` along
#' with arguments for which locations, cohorts, and ages to target. Includes
#' locations which were not present in the original simulated observations,
#' namely the State and County levels.
#'
#' @format A `[data.table()]` with 1008 rows and 7 columns:
#'  - `obs_c_id`, a number, the target observation/combination ID (primary key)
#'  - `loc_id`, a string, the location ID
#'  - `age`, a number, the age
#'  - `cohort`, a number, the birth cohort
#'  - `dose`, a number, the vaccine dose (1 or 2)
#'  - `weight`, a number, the weight of the prediction component (always 1)
#'  - `loc_c_id`, a number, the compiled location ID
"target_sim"

#' @title Example Coverage Predictions
#'
#' @description
#' A dataset containing predicted vaccine coverage probabilities generated
#' by calling [predict()] on `fit_sim` with `target_sim` as the target.
#'
#' @format An object of class `imugap_predict` wrapping:
#'  - `draws`, a 3D array of predicted draws with dimensions `[iteration, chain, variable]`
#'  - `target`, a `[data.table()]` containing target population parameters matching the variables
"predict_sim"
