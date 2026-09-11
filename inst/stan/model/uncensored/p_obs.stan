vector[n_obs_uncensored] p_obs_uncensored = compute_p_obs(
  n_obs_uncensored,
  n_weights_uncensored,
  phi,
  phi_lookup_uncensored,
  unrolled_dose_probs,
  cdf_lookup_uncensored,
  weights_uncensored,
  obs_map_uncensored
);
