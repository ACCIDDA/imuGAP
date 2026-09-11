vector[n_doses * n_yr] unrolled_dose_probs = unrolled_dose(n_yr, n_doses, dose_sched, lambda_raw);

#include model/uncensored/p_obs.stan
#include model/right/p_obs.stan
#include model/left/p_obs.stan
