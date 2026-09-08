  #include transformed_data/common_indices.stan

  array[n_weights_uncensored] int<lower=1> phi_lookup_uncensored = compute_phi_lookup(weights_cohort_uncensored, weights_location_uncensored, n_cohort);
  array[n_weights_right] int<lower=1> phi_lookup_right = compute_phi_lookup(weights_cohort_right, weights_location_right, n_cohort);
  array[n_weights_left] int<lower=1> phi_lookup_left = compute_phi_lookup(weights_cohort_left, weights_location_left, n_cohort);

  // Equivalent 1:n_cohort, for time trends
  vector[n_cohort] cohort_shift_counter = linspaced_vector(n_cohort, 1, n_cohort);

  // Direct mapping from each non-root offset index (1 .. n_locs - 1) to its layer index (1 .. n_layers - 1)
  array[n_locs - 1] int<lower=1, upper=n_layers - 1> loc_layer_idx;
  for (k in 1:(n_layers - 1)) {
    loc_layer_idx[(layer_bounds[1, k + 1] - 1):(layer_bounds[2, k + 1] - 1)] = rep_array(k, layer_sizes[k + 1]);
  }
