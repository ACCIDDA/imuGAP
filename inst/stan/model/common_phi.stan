vector[n_doses * n_intervals] unrolled_dose_probs =
  unrolled_dose(n_intervals, n_doses, dt_vec, dose_sched, lambda_raw);

#include model/uncensored/p_obs.stan
#include model/right/p_obs.stan
#include model/left/p_obs.stan
