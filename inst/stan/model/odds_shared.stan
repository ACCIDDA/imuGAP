// 1. Calculate sum-to-zero county offsets (state level) and school offsets (parent county level)
vector[n_cnty] off_cnty = Q_cnty * off_cnty_raw;
row_vector[n_sch] off_sch = (Q_sch * off_sch_raw)';

// 2. Calculate school-level probabilities for all cohorts
vector[n_cohort] logit_phi_st = bs * beta_bs;
matrix[n_cohort, n_sch] phi_sch;

row_vector[n_sch] total_shift = off_sch;
for (c in 1:n_cnty) {
  total_shift[cnty_map[1, c]:cnty_map[2, c]] += off_cnty[c];
}

for (c in 1:n_cohort) {
  phi_sch[c, ] = inv_logit(logit_phi_st[c] + total_shift);
}

// 3. Roll up school probabilities to county level
matrix[n_cohort, n_cnty] phi_cnty;
for (c in 1:n_cohort) {
  for (m in 1:n_cnty) {
    phi_cnty[c, m] = dot_product(
      phi_sch[c, cnty_map[1, m]:cnty_map[2, m]],
      school_w_in_cnty[cnty_map[1, m]:cnty_map[2, m]]
    );
  }
}

// 4. Roll up county probabilities to state level
vector[n_cohort] phi_state;
for (c in 1:n_cohort) {
  phi_state[c] = dot_product(phi_cnty[c, ], cnty_w_in_state);
}

// 5. Combine into complete phi lookup vector [state; counties; schools]
vector[(1 + n_cnty + n_sch) * n_cohort] phi = append_row(
  append_row(phi_state, to_vector(phi_cnty)),
  to_vector(phi_sch)
);

// 6. Likelihood observation probabilities (identical to v6)
vector[n_doses * n_yr] unrolled_dose_probs = unrolled_dose(n_yr, n_doses, dose_sched, lambda_raw, epsilon_p);
vector[n_weights] weighted = (1 - phi[phi_lookup]) .* unrolled_dose_probs[cdf_lookup] .* weights;
for (obs_i in 1:n_obs) {
  p_obs[obs_i] = sum(weighted[obs_map[1, obs_i]:obs_map[2, obs_i]]);
}
