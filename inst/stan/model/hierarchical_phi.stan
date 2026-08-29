vector[n_cohort] logit_phi_st = bs * beta_bs;

vector[n_locs] logit_phi_loc;
logit_phi_loc[1] = 0.0;
for (p in 1:n_parent_locs) {
  int st = parent_child_bounds[1, p];
  int en = parent_child_bounds[2, p];
  logit_phi_loc[st:en] = logit_phi_loc[parent_loc_id[p]] + off_layer[(st - 1):(en - 1)];
}

matrix[n_cohort, n_locs] logit_phi_mat = rep_matrix(logit_phi_st, n_locs) + rep_matrix(to_row_vector(logit_phi_loc), n_cohort);
vector[n_cohort * n_locs] phi = to_vector(inv_logit(logit_phi_mat));

vector[n_doses * n_yr] unrolled_dose_probs = unrolled_dose(n_yr, n_doses, dose_sched, lambda_raw, epsilon_p);
vector[n_weights] weighted = (1 - phi[phi_lookup]) .* unrolled_dose_probs[cdf_lookup] .* weights;
for (obs_i in 1:n_obs) {
  p_obs[obs_i] = sum(weighted[obs_map[1,obs_i]:obs_map[2,obs_i]]);
}
