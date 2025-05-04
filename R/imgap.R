
imgap_options <- function(
  total_years = 30, county_demography, state_demography 
) {

}

stan_options <- function(...) {

}

#' @title Immunity: Geographic & Age-based Projection, `imgap`
#'
#' @description This function estimates current coverage, by age and location.
#'
#' @param x a 
#' @param y Numeric vector of output values.
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
#' @export
#' 
#' @examples
#' 
imgap <- function(
  x, y,
  state_model = c("ar", "noisy"),
  cnty_model = c("ar", "noisy", "static"),
  schl_model = c("ar", "noisy", "static"),
  imgap_options = imgap_options(),
  stan_options = stan_options()
) {
  model <- standmodels[[match.arg(model)]]

# x should be school / county demographic data: data.table(school_id, county_id, year, age_start, age_end, dose, sample_n, population)
# school_ids should ultimately be unique globally
  x_dt <- as.data.table(x)

# y should be observed counts by county, school, year, age_start, age_end;
# if school_id, county_id is NA, that means statewide
# if county_id is NA, that means county-wide
  y_dt <- as.data.table(y)

  dat_stan <- list(n_yr = total_years,
                 n_cnty = x_dt[, length(unique(county_id))],
                 n_school = x_dt[, length(unique(school_id))],
                 n_state_obs = y_dt[is.na(school_id) & is.na(county_id), .N],
                 n_sch_obs = y_dt[!is.na(school_id), .N],
                 cnty_start_index = x_dt[, min(), keyby = county_id],
                 cnty_end_index = x_dt[, max(), keyby = county_id],
                 sch_wts = sch_wts_stan,
                 pop_wts = pop_wts,
                 y_sch = school_stan %>% dplyr::select(Up_to_date_count, Total_enrollment),
                 y_sch_id = school_stan$sch_id,
                 y_sch_yr = school_stan$Year - 1997,
                 y_state = y_state %>% dplyr::select(X, N),
                 y_state_pop = y_state$pop,
                 y_state_grades = grade_wts)


  out <- rstan::sampling(model, data = standata, ...)
  return(out)
}