  // convert to lookup spans for convenience
  array[2, n_obs] int obs_map = bounds_to_range(obs_to_weights_bounds, n_weights);
  
  // Equivalent 1:n_cohort, for time trends
  vector[n_cohort] cohort_shift_counter = linspaced_vector(n_cohort, 1, n_cohort);

  array[n_weights] int<lower=1> phi_lookup;
  array[n_weights] int<lower=1> cdf_lookup;
  // because integer arrays don't support broadcasting ...
  // unroll phi and cdf objects to support vectorization
  for (weight_i in 1:n_weights) {
    // phi ordered by location then cohort
    phi_lookup[weight_i] = weights_cohort[weight_i] + (weights_location[weight_i] - 1) * n_cohort;
    // ordered by dose then observation age
    cdf_lookup[weight_i] = weights_obs_age[weight_i] + (weights_dose[weight_i] - 1) * n_ages;
  }
