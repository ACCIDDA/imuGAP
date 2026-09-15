vector[n_obs_unmixed_left] p_obs_unmixed_left;
if (n_obs_unmixed_left > 0) {
  p_obs_unmixed_left =
    (1.0 - phi[phi_lookup_unmixed_left]) .* unrolled_dose_probs[cdf_lookup_unmixed_left];
}

vector[n_obs_mixed_left] p_obs_mixed_left;
if (n_obs_mixed_left > 0) {
  p_obs_mixed_left = compute_p_obs(
    n_obs_mixed_left,
    n_weights_mixed_left,
    phi,
    phi_lookup_mixed_left,
    unrolled_dose_probs,
    cdf_lookup_mixed_left,
    weights_mixed_left,
    obs_map_mixed_left
  );
}
