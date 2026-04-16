
# observations may be right-censored
# observation data is assumed ordered uncensored, then right censored
# so n_uncensored_obs == n_obs, all observations are uncensored
# number of uncensored observations
int<lower = 0, upper = n_obs> n_uncensored_obs;
