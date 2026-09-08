
  // STRUCTURAL DEFINITIONS

  int<lower=1> n_yr; // number of years to model for each cohort - should be at least year of oldest observation
  int<lower=1> n_cohort; // number of birth year cohorts
  
  // dose schedules
  int<lower=1> n_doses;
  matrix<lower=0, upper=1>[n_yr, n_doses] dose_sched;

  // DATA DEFINITIONS

  int<lower=0> n_obs_uncensored;
  array[n_obs_uncensored] int<lower=0> y_obs_uncensored;
  array[n_obs_uncensored] int<lower=0> y_smp_uncensored;
  int<lower=0> n_weights_uncensored;
  array[n_obs_uncensored] int<lower=1, upper=n_weights_uncensored> obs_to_weights_bounds_uncensored;
  array[n_weights_uncensored] int<lower=1, upper=n_cohort> weights_cohort_uncensored;
  array[n_weights_uncensored] int<lower=1, upper=n_yr> weights_life_year_uncensored;
  array[n_weights_uncensored] int<lower=1, upper=n_doses> weights_dose_uncensored;
  vector<lower=0, upper=1>[n_weights_uncensored] weights_uncensored;

  int<lower=0> n_obs_right;
  array[n_obs_right] int<lower=0> y_obs_right;
  array[n_obs_right] int<lower=0> y_smp_right;
  int<lower=0> n_weights_right;
  array[n_obs_right] int<lower=1, upper=n_weights_right> obs_to_weights_bounds_right;
  array[n_weights_right] int<lower=1, upper=n_cohort> weights_cohort_right;
  array[n_weights_right] int<lower=1, upper=n_yr> weights_life_year_right;
  array[n_weights_right] int<lower=1, upper=n_doses> weights_dose_right;
  vector<lower=0, upper=1>[n_weights_right] weights_right;

  int<lower=0> n_obs_left;
  array[n_obs_left] int<lower=0> y_obs_left;
  array[n_obs_left] int<lower=0> y_smp_left;
  int<lower=0> n_weights_left;
  array[n_obs_left] int<lower=1, upper=n_weights_left> obs_to_weights_bounds_left;
  array[n_weights_left] int<lower=1, upper=n_cohort> weights_cohort_left;
  array[n_weights_left] int<lower=1, upper=n_yr> weights_life_year_left;
  array[n_weights_left] int<lower=1, upper=n_doses> weights_dose_left;
  vector<lower=0, upper=1>[n_weights_left] weights_left;

  // run mode: 0 = estimation, 1 = prediction
  int<lower=0, upper=1> predict_mode;
