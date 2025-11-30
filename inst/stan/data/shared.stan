
  // STRUCTURAL DEFINITIONS

  int<lower=1> n_yr; // number of years to model for each cohort - should be at least year of oldest observation
  int<lower=1> n_cohort; // number of birth year cohorts
  int<lower=1> n_sch; // number of schools
  
  // dose schedules
  int<lower=1> n_doses;
  matrix<lower=0, upper=1>[n_yr, n_doses] dose_sched;

  // DATA DEFINITIONS

  int<lower=1> n_obs;
  int<lower=0> y_obs[n_obs];
  int<lower=0> y_smp[n_obs];
  // have school id ranges for observations & for doses; school id 0 == statewide?
  // int obs_sch_id_bounds[n_obs];

  int<lower=n_obs> n_weights;
  array[n_obs] int<lower=1, upper=n_weights> obs_to_weights_bounds; // each entry is the start of the range

  int<lower=1,upper=n_sch+1> weights_school[n_weights];
  int<lower=1,upper=n_cohort> weights_cohort[n_weights];
  int<lower=1,upper=n_yr> weights_life_year[n_weights];
  int<lower=1,upper=n_doses> weights_dose[n_weights];

  vector<lower=0,upper=1>[n_weights] weights; // contribution of this (school, cohort, year, dose) to an observation

  // maps schools to county
  int n_cnty;
  array[n_cnty] int<lower=1, upper=n_sch> cnty_bounds; // which schools indices start each county

  // run mode: 0 = estimation, 1 = prediction
  int<lower=0, upper=1> predict_mode;
