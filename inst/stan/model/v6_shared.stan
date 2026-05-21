vector[n_cohort] logit_phi_st = bs * beta_bs;
row_vector[n_sch] shift = off_sch;
for (c in 1:n_cnty) {
  shift[cnty_map[1,c]:cnty_map[2,c]] += off_cnty[c];
}
vector[(1+ n_cnty + n_sch) * n_cohort] phi = append_row(append_row(
  inv_logit(logit_phi_st),
  to_vector(inv_logit(
    rep_matrix(logit_phi_st, n_cnty) + rep_matrix(off_cnty, n_cohort)
  ))),
  to_vector(inv_logit(
    rep_matrix(logit_phi_st, n_sch) + rep_matrix(shift, n_cohort)
  ))
);
vector[n_doses * n_yr] unrolled_dose_probs = unrolled_dose(n_yr, n_doses, dose_sched, lambda_raw, epsilon_p);
vector[n_weights] weighted = (1 - phi[phi_lookup]) .* unrolled_dose_probs[cdf_lookup] .* weights;
for (obs_i in 1:n_obs) {
  p_obs[obs_i] = sum(weighted[obs_map[1,obs_i]:obs_map[2,obs_i]]);
}
