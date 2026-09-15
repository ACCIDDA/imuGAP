int<lower=0> n_obs_unmixed_uncensored;
array[n_obs_unmixed_uncensored] int<lower=0> y_obs_unmixed_uncensored;
array[n_obs_unmixed_uncensored] int<lower=0> y_smp_unmixed_uncensored;
array[n_obs_unmixed_uncensored] int<lower=1, upper=n_cohort> w_cohort_unmixed_uncensored;
array[n_obs_unmixed_uncensored] int<lower=1, upper=n_yr> w_age_unmixed_uncensored;
array[n_obs_unmixed_uncensored] int<lower=1, upper=n_doses> w_dose_unmixed_uncensored;

int<lower=0> n_obs_mixed_uncensored;
array[n_obs_mixed_uncensored] int<lower=0> y_obs_mixed_uncensored;
array[n_obs_mixed_uncensored] int<lower=0> y_smp_mixed_uncensored;
int<lower=0> n_weights_mixed_uncensored;
array[n_obs_mixed_uncensored] int<lower=1, upper=n_weights_mixed_uncensored> obs_bounds_mixed_uncensored;
array[n_weights_mixed_uncensored] int<lower=1, upper=n_cohort> w_cohort_mixed_uncensored;
array[n_weights_mixed_uncensored] int<lower=1, upper=n_yr> w_age_mixed_uncensored;
array[n_weights_mixed_uncensored] int<lower=1, upper=n_doses> w_dose_mixed_uncensored;
vector<lower=0, upper=1>[n_weights_mixed_uncensored] weights_mixed_uncensored;
