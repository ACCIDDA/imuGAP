array[2, n_obs_mixed_left] int obs_map_mixed_left =
  bounds_to_range(obs_bounds_mixed_left, n_weights_mixed_left);
array[n_obs_unmixed_left] int<lower=1> cdf_lookup_unmixed_left =
  compute_cdf_lookup(w_age_unmixed_left, w_dose_unmixed_left, n_intervals, age_to_interval_map);
array[n_weights_mixed_left] int<lower=1> cdf_lookup_mixed_left =
  compute_cdf_lookup(w_age_mixed_left, w_dose_mixed_left, n_intervals, age_to_interval_map);
