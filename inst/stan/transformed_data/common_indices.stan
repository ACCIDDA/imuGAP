  array[2, n_obs_uncensored] int obs_map_uncensored = bounds_to_range(obs_to_weights_bounds_uncensored, n_weights_uncensored);
  array[2, n_obs_right] int obs_map_right = bounds_to_range(obs_to_weights_bounds_right, n_weights_right);
  array[2, n_obs_left] int obs_map_left = bounds_to_range(obs_to_weights_bounds_left, n_weights_left);

  array[n_weights_uncensored] int<lower=1> cdf_lookup_uncensored = compute_cdf_lookup(weights_life_year_uncensored, weights_dose_uncensored, n_yr);
  array[n_weights_right] int<lower=1> cdf_lookup_right = compute_cdf_lookup(weights_life_year_right, weights_dose_right, n_yr);
  array[n_weights_left] int<lower=1> cdf_lookup_left = compute_cdf_lookup(weights_life_year_left, weights_dose_left, n_yr);
