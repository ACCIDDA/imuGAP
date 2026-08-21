vector<lower=0,upper=1>[n_obs] p_obs = calculate_ps(
  eval_times, tau, lambda, // terms for unrolled_phi_pdf
  phi, phi_lookup, cdf_lookup, weights, // terms for weighted
  n_obs, obs_map // map to create p_obs
);
