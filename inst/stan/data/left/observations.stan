int<lower=0> n_obs_left;
array[n_obs_left] int<lower=0> y_obs_left;
array[n_obs_left] int<lower=0> y_smp_left;
int<lower=0> n_weights_left;
array[n_obs_left] int<lower=1, upper=n_weights_left> obs_to_weights_bounds_left;
array[n_weights_left] int<lower=1, upper=n_cohort> weights_cohort_left;
array[n_weights_left] int<lower=1, upper=n_yr> weights_life_year_left;
array[n_weights_left] int<lower=1, upper=n_doses> weights_dose_left;
vector<lower=0, upper=1>[n_weights_left] weights_left;
