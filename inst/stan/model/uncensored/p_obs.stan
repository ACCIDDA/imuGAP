vector[n_obs_unmixed_uncensored] p_obs_unmixed_uncensored;
if (n_obs_unmixed_uncensored > 0) {
  p_obs_unmixed_uncensored =
    (1.0 - phi[phi_lookup_unmixed_uncensored]) .* unrolled_dose_probs[cdf_lookup_unmixed_uncensored];
}

vector[n_obs_mixed_uncensored] p_obs_mixed_uncensored;
if (n_obs_mixed_uncensored > 0) {
  p_obs_mixed_uncensored = compute_p_obs(
    n_obs_mixed_uncensored,
    n_weights_mixed_uncensored,
    phi,
    phi_lookup_mixed_uncensored,
    unrolled_dose_probs,
    cdf_lookup_mixed_uncensored,
    weights_mixed_uncensored,
    obs_map_mixed_uncensored
  );
}
