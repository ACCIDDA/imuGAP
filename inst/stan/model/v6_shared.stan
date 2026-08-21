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
