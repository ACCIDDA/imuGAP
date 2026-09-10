int<lower=0> n_obs_uncensored;
array[n_obs_uncensored] int<lower=0> y_obs_uncensored;
array[n_obs_uncensored] int<lower=0> y_smp_uncensored;
int<lower=0> n_weights_uncensored;
array[n_obs_uncensored] int<lower=1, upper=n_weights_uncensored> obs_to_weights_bounds_uncensored;
array[n_weights_uncensored] int<lower=1, upper=n_cohort> weights_cohort_uncensored;
array[n_weights_uncensored] int<lower=1, upper=n_yr> weights_life_year_uncensored;
array[n_weights_uncensored] int<lower=1, upper=n_doses> weights_dose_uncensored;
vector<lower=0, upper=1>[n_weights_uncensored] weights_uncensored;
