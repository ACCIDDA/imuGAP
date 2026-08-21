
  // STRUCTURAL DEFINITIONS

  int<lower=1> n_yr; // number of years to model for each cohort - should be at least year of oldest observation
  int<lower=1> n_cohort; // number of birth year cohorts

  // Location hierarchy: arbitrary user-specified layers
  int<lower=1> n_locs; // total number of locations (root + all sub-locations)
  int<lower=1> n_layers; // number of hierarchy layers (depth)
  array[n_layers] int<lower=1> layer_sizes; // number of locations in each layer
  array[2, n_layers] int<lower=1> layer_bounds; // start and end location indices for each layer
  array[n_locs] int<lower=0> parent_id_map; // parent location index (0 for root location)
  array[n_locs] int<lower=1> layer_id_map; // layer index (1..n_layers) for each location

  // dose schedules
  int<lower=1> n_doses;
  matrix<lower=0, upper=1>[n_yr, n_doses] dose_sched;

  // DATA DEFINITIONS

  int<lower=1> n_obs;
  array[n_obs] int<lower=0> y_obs;
  array[n_obs] int<lower=0> y_smp;

  int<lower=n_obs> n_weights;
  array[n_obs] int<lower=1, upper=n_weights> obs_to_weights_bounds; // each entry is the start of the range

  array[n_weights] int<lower=1, upper=n_locs> weights_location;
  array[n_weights] int<lower=1, upper=n_cohort> weights_cohort;
  array[n_weights] int<lower=1, upper=n_yr> weights_life_year;
  array[n_weights] int<lower=1, upper=n_doses> weights_dose;

  vector<lower=0, upper=1>[n_weights] weights; // contribution of this (location, cohort, year, dose) to an observation

  // run mode: 0 = estimation, 1 = prediction
  int<lower=0, upper=1> predict_mode;
