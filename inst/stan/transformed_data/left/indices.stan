array[2, n_obs_left] int obs_map_left = bounds_to_range(obs_to_weights_bounds_left, n_weights_left);
array[n_weights_left] int<lower=1> cdf_lookup_left = compute_cdf_lookup(weights_life_year_left, weights_dose_left, n_yr, n_doses);
