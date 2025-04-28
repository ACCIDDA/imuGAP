  matrix[n_cnty, n_yr] lambda_c; // county-level coverage
  // total coverage lambda: always starts based off state 
  matrix[n_school, n_yr] lambda = rep_matrix(mu, n_school);
  int school_rep_size[] = cnty_end_index - cnty_start_index + 1;