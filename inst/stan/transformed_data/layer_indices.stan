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

  // Precomputed block-diagonal QR basis for weighted sum-to-zero sibling offsets
  int n_unconstrained_offsets = (n_locs - 1) - n_parent_locs;
  matrix[n_locs - 1, n_unconstrained_offsets] qr_basis = rep_matrix(0.0, n_locs - 1, n_unconstrained_offsets);
  int col_offset = 0;
  for (p in 1:n_parent_locs) {
    int st = parent_child_bounds[1, p];
    int en = parent_child_bounds[2, p];
    int K = en - st + 1;
    vector[K] pop_slice = loc_population[st:en];
    real sum_pop = sum(pop_slice);
    vector[K] w;
    if (sum_pop > 0) {
      w = pop_slice / sum_pop;
    } else {
      w = rep_vector(1.0 / K, K);
    }
    matrix[K, K - 1] Q_star = get_weighted_qr_basis(w);
    qr_basis[(st - 1):(en - 1), (col_offset + 1):(col_offset + K - 1)] = Q_star;
    col_offset += (K - 1);
  }
