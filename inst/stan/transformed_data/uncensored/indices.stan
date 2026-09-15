array[2, n_obs_mixed_uncensored] int obs_map_mixed_uncensored =
  bounds_to_range(obs_bounds_mixed_uncensored, n_weights_mixed_uncensored);
array[n_obs_unmixed_uncensored] int<lower=1> cdf_lookup_unmixed_uncensored =
  compute_cdf_lookup(w_age_unmixed_uncensored, w_dose_unmixed_uncensored, n_intervals, age_to_interval_map);
array[n_weights_mixed_uncensored] int<lower=1> cdf_lookup_mixed_uncensored =
  compute_cdf_lookup(w_age_mixed_uncensored, w_dose_mixed_uncensored, n_intervals, age_to_interval_map);
