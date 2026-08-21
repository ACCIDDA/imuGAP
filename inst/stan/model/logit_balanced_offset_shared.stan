// 1. Compute state-level logit target
vector[n_cohort] logit_phi_st = bs * beta_bs;
vector[n_cohort] phi_state = inv_logit(logit_phi_st);

matrix[n_cohort, n_cnty] phi_cnty;
matrix[n_cohort, n_sch] phi_sch;

row_vector[n_cnty] cnty_w_row = to_row_vector(cnty_w_in_state);

// 2. Compute balanced offsets for counties and schools
for (c in 1:n_cohort) {
  real p_st_target = phi_state[c];
  real st_logit = logit_phi_st[c];

  // State -> County level balanced moving offset theta_cnty
  real theta_cnty = 0.0;
  for (iter in 1:6) {
    row_vector[n_cnty] p_c = inv_logit(st_logit + theta_cnty + off_cnty);
    real f_val = dot_product(cnty_w_row, p_c) - p_st_target;
    real f_deriv = dot_product(cnty_w_row, p_c .* (1.0 - p_c));
    if (f_deriv > 1e-12) {
      theta_cnty = theta_cnty - f_val / f_deriv;
    }
  }

  for (m in 1:n_cnty) {
    phi_cnty[c, m] = inv_logit(st_logit + theta_cnty + off_cnty[m]);

    // County -> School level balanced moving offset theta_sch
    int sch_start_idx = cnty_map[1, m];
    int sch_end_idx = cnty_map[2, m];
    int n_sch_c = sch_end_idx - sch_start_idx + 1;

    row_vector[n_sch_c] sch_w = to_row_vector(school_w_in_cnty[sch_start_idx:sch_end_idx]);
    row_vector[n_sch_c] sch_off = off_sch[sch_start_idx:sch_end_idx];
    real p_cnty_target = phi_cnty[c, m];
    real cnty_logit = logit(p_cnty_target);

    real theta_sch = 0.0;
    for (iter in 1:6) {
      row_vector[n_sch_c] p_s = inv_logit(cnty_logit + theta_sch + sch_off);
      real f_val = dot_product(sch_w, p_s) - p_cnty_target;
      real f_deriv = dot_product(sch_w, p_s .* (1.0 - p_s));
      if (f_deriv > 1e-12) {
        theta_sch = theta_sch - f_val / f_deriv;
      }
    }

    for (s_sub in 1:n_sch_c) {
      int s_idx = sch_start_idx + s_sub - 1;
      phi_sch[c, s_idx] = inv_logit(cnty_logit + theta_sch + sch_off[s_sub]);
    }
  }
}

// 3. Combine into complete phi lookup vector [state; counties; schools]
vector[(1 + n_cnty + n_sch) * n_cohort] phi = append_row(
  append_row(phi_state, to_vector(phi_cnty)),
  to_vector(phi_sch)
);
