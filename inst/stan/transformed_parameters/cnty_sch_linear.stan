
  row_vector[n_sch] shift = off_sch;
  row_vector[n_sch] shift_trend = trend_sch;

  for (c in 1:n_cnty) {
    shift[cnty_map[1,c]:cnty_map[2,c]] += off_cnty[c]; // add county offsets to relevant school offsets
    shift_trend[cnty_map[1,c]:cnty_map[2,c]] += trend_cnty[c]; // add county trends to relevant school trends
  }
  
  // school-level phi by year, converted to column major vector
  // so it's school n, cohort 1 => n_cohort; school n+1, ...
  // TODO double check mapping from matrix to vector
  vector<lower=0, upper=1>[(n_sch + 1) * n_cohort] phi = append_row(
    to_vector(inv_logit(
      rep_matrix(logit_phi_st, n_sch) + // apply state terms for every cohort across schools => n_cohort x 1 => n_cohort x n_sch
      rep_matrix(shift, n_cohort) + // apply base school terms across every cohort => 1 x n_sch => n_cohort x n_sch
      element_mult_expand(cohort_shift_counter, shift_trend) // expand trend across cohorts
    )), // n_cohort x n_sch => n_cohort * n_sch
    inv_logit(logit_phi_st)
  );
