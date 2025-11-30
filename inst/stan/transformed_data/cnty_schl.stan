  // convert to lookup spans for convenience
  array[2, n_cnty] int cnty_map = bounds_to_range(cnty_bounds, n_sch);
  array[2, n_obs] int obs_map = bounds_to_range(obs_to_weights_bounds, n_weights);
  
  // Equivalent 1:n_cohort, for time trends
  vector[n_cohort] cohort_shift_counter = linspaced_vector(n_cohort, 1, n_cohort);

  int<lower=1> phi_lookup[n_weights];
  int<lower=1> cdf_lookup[n_weights];
  // because integer arrays don't support broadcasting ...
  // unroll phi and cdf objects to support vectorization
  for (weight_i in 1:n_weights) {
    // phi ordered by school then cohort
    phi_lookup[weight_i] = weights_cohort[weight_i] + (weights_school[weight_i] - 1) * n_cohort;
    // ordered by dose then life year
    cdf_lookup[weight_i] = weights_life_year[weight_i] + (weights_dose[weight_i] - 1) * n_yr;
  }
