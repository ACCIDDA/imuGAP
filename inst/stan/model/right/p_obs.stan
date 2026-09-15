vector[n_obs_right] p_obs_right;
if (n_obs_unmixed_right > 0) {
  p_obs_right[unmixed_orig_order_right] =
    (1.0 - phi[phi_lookup_unmixed_right]) .* unrolled_dose_probs[cdf_lookup_unmixed_right];
}
if (n_obs_mixed_right > 0) {
  p_obs_right[mixed_orig_order_right] = compute_p_obs(
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
