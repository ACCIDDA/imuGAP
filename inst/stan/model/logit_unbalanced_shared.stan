// 1. Calculate school-level and county-level probabilities on the additive logit scale
vector[n_cohort] logit_phi_st = bs * beta_bs;
row_vector[n_sch] shift = off_sch;
for (c in 1:n_cnty) {
  shift[cnty_map[1, c]:cnty_map[2, c]] += off_cnty[c];
}

vector[n_cohort] phi_state = inv_logit(logit_phi_st);
matrix[n_cohort, n_cnty] phi_cnty = inv_logit(
  rep_matrix(logit_phi_st, n_cnty) + rep_matrix(off_cnty, n_cohort)
);
matrix[n_cohort, n_sch] phi_sch = inv_logit(
  rep_matrix(logit_phi_st, n_sch) + rep_matrix(shift, n_cohort)
);

// 2. Combine into complete phi lookup vector [state; counties; schools]
vector[(1 + n_cnty + n_sch) * n_cohort] phi = append_row(
  append_row(phi_state, to_vector(phi_cnty)),
  to_vector(phi_sch)
);
