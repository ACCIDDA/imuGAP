array[2, n_obs_right] int obs_map_right = bounds_to_range(obs_to_weights_bounds_right, n_weights_right);
array[n_weights_right] int<lower=1> cdf_lookup_right = compute_cdf_lookup(weights_life_year_right, weights_dose_right, n_yr, n_doses);
