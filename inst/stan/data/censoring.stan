
# observations may be right-censored
# observation data is assumed ordered right censored first, then uncensored
# so n_censor_obs == 0, all observations are uncensored
# number of censored observations
int<lower = 0, upper = n_obs> n_censor_obs;
