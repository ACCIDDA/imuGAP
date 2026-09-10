int<lower=0> n_obs_right;
array[n_obs_right] int<lower=0> y_obs_right;
array[n_obs_right] int<lower=0> y_smp_right;
int<lower=0> n_weights_right;
array[n_obs_right] int<lower=1, upper=n_weights_right> obs_to_weights_bounds_right;
array[n_weights_right] int<lower=1, upper=n_cohort> weights_cohort_right;
array[n_weights_right] int<lower=1, upper=n_yr> weights_life_year_right;
array[n_weights_right] int<lower=1, upper=n_doses> weights_dose_right;
vector<lower=0, upper=1>[n_weights_right] weights_right;
