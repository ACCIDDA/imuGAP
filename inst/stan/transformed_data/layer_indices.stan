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
    // ordered by dose then life year
    cdf_lookup[weight_i] = weights_life_year[weight_i] + (weights_dose[weight_i] - 1) * n_yr;
  }

  // Direct mapping from each non-root offset index (1 .. n_locs - 1) to its layer index (1 .. n_layers - 1)
  array[n_locs - 1] int<lower=1, upper=n_layers - 1> loc_layer_idx;
  for (k in 1:(n_layers - 1)) {
    loc_layer_idx[(layer_bounds[1, k + 1] - 1):(layer_bounds[2, k + 1] - 1)] = rep_array(k, layer_sizes[k + 1]);
  }
