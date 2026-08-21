
// STRUCTURAL DEFINITIONS

// TIME RELATED
int<lower=1> n_cohort; // number of birth year cohorts
int<lower=1> n_ages; // number of observation times
positive_ordered[n_ages] obs_age; // the unique observation ages (time in life)

// DOSE RELATED
int<lower=1> n_doses;
positive_ordered[n_doses] tau; // times of eligibility for each dose

// LOCATION RELATED
int<lower=1> n_layers; // number of layers, including root; e.g. state-county-school would be 3
int<lower=1> n_locs; // total number of locations, including root
int<lower=0> n_parent_locs; // number of locations which have offspring
array[n_layers - 1] int<lower=1> layer_sizes; // how many offspring for whole layer; final layer has no offspring
array[n_parent_locs] int<lower=1,upper=n_locs> layer_bounds;

// DATA DEFINITIONS
int<lower=1> n_obs;
array[n_obs] int<lower=0> y_obs;
array[n_obs] int<lower=0> y_smp;

int<lower=n_obs> n_weights;
array[n_obs] int<lower=1, upper=n_weights> obs_to_weights_bounds; // each entry is the start of the range

// indicies for each of the elements of a weight
array[n_weights] int<lower=1> weights_location;
array[n_weights] int<lower=1,upper=n_cohort> weights_cohort;
array[n_weights] int<lower=1,upper=n_ages> weights_obs_age;
array[n_weights] int<lower=1,upper=n_doses> weights_dose;

vector<lower=0,upper=1>[n_weights] weights; // contribution of this (school, cohort, year, dose) to an observation

// run mode: 0 = estimation, 1 = prediction
int<lower=0, upper=1> predict_mode;
