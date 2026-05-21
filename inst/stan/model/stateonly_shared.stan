vector[n_cohort] logit_phi_st = bs * beta_bs;
vector[n_cohort] phi = inv_logit(logit_phi_st);
vector[n_doses * n_yr] unrolled_dose_probs = unrolled_dose(n_yr, n_doses, dose_sched, lambda_raw, epsilon_p);
vector[n_weights] weighted = (1 - phi[phi_lookup]) .* unrolled_dose_probs[cdf_lookup] .* weights;
for (obs_i in 1:n_obs) {
  p_obs[obs_i] = sum(weighted[obs_map[1,obs_i]:obs_map[2,obs_i]]);
}
