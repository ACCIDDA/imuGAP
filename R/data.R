#' Simulated location hierarchy
#'
#' A 3-layer location hierarchy (1 state, 3 counties, 24 schools) for
#' testing and examples.
#'
#' @format A [data.table::data.table] with columns:
#' \describe{
#'   \item{loc_id}{Location identifier (character)}
#'   \item{parent_id}{Parent location identifier (character; NA for root)}
#' }
"locations_sim"

#' Simulated observations
#'
#' Simulated vaccination observation data matching [locations_sim].
#'
#' @format A [data.table::data.table] with columns:
#' \describe{
#'   \item{obs_id}{Observation identifier (integer)}
#'   \item{positive}{Number of vaccinated individuals}
#'   \item{sample_n}{Total sample size}
#' }
"observations_sim"

#' Simulated observation populations
#'
#' Observation-to-population mapping for [observations_sim], linking each
#' observation to location, cohort, age, dose, and weight.
#'
#' @format A [data.table::data.table] with columns:
#' \describe{
#'   \item{obs_id}{Observation identifier matching [observations_sim]}
#'   \item{loc_id}{Location identifier matching [locations_sim]}
#'   \item{cohort}{Cohort index (positive integer)}
#'   \item{age}{Age index (positive integer)}
#'   \item{dose}{Dose number (1 or 2)}
#'   \item{weight}{Relative contribution to the observation (0, 1]}
#' }
"populations_sim"
