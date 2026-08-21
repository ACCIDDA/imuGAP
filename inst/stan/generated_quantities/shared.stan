
vector<lower=0,upper=1>[n_obs] p_gen = calculate_ps(
  eval_times, tau, exp(lambda_raw), // terms for unrolled_phi_pdf
  phi, phi_lookup, cdf_lookup, weights,
  n_obs, obs_map
);
