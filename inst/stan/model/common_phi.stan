vector[n_obs_uncensored] p_obs_uncensored = compute_p_obs(n_obs_uncensored, n_weights_uncensored, phi, phi_lookup_uncensored, unrolled_dose_probs, cdf_lookup_uncensored, weights_uncensored, obs_map_uncensored);
vector[n_obs_right] p_obs_right = compute_p_obs(n_obs_right, n_weights_right, phi, phi_lookup_right, unrolled_dose_probs, cdf_lookup_right, weights_right, obs_map_right);
vector[n_obs_left] p_obs_left = compute_p_obs(n_obs_left, n_weights_left, phi, phi_lookup_left, unrolled_dose_probs, cdf_lookup_left, weights_left, obs_map_left);
