vector[n_obs_unmixed_right] p_obs_unmixed_right;
if (n_obs_unmixed_right > 0) {
  p_obs_unmixed_right =
    (1.0 - phi[phi_lookup_unmixed_right]) .* unrolled_dose_probs[cdf_lookup_unmixed_right];
}

vector[n_obs_mixed_right] p_obs_mixed_right;
if (n_obs_mixed_right > 0) {
  p_obs_mixed_right = compute_p_obs(
    n_obs_mixed_right,
    n_weights_mixed_right,
    phi,
    phi_lookup_mixed_right,
    unrolled_dose_probs,
    cdf_lookup_mixed_right,
    weights_mixed_right,
    obs_map_mixed_right
  );
}
