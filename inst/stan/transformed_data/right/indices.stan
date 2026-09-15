array[2, n_obs_mixed_right] int obs_map_mixed_right =
  bounds_to_range(obs_bounds_mixed_right, n_weights_mixed_right);
array[n_obs_unmixed_right] int<lower=1> cdf_lookup_unmixed_right =
  compute_cdf_lookup(w_age_unmixed_right, w_dose_unmixed_right, n_intervals, age_to_interval_map);
array[n_weights_mixed_right] int<lower=1> cdf_lookup_mixed_right =
  compute_cdf_lookup(w_age_mixed_right, w_dose_mixed_right, n_intervals, age_to_interval_map);
