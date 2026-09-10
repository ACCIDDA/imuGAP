array[2, n_obs_uncensored] int obs_map_uncensored = bounds_to_range(obs_to_weights_bounds_uncensored, n_weights_uncensored);
array[n_weights_uncensored] int<lower=1> cdf_lookup_uncensored = compute_cdf_lookup(weights_life_year_uncensored, weights_dose_uncensored, n_yr, n_doses);
