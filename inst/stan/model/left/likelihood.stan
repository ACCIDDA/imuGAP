if (n_obs_unmixed_left > 0) {
  target += reduce_sum(
    unmixed_left_partial_lcdf,
    y_obs_unmixed_left,
    max(1, n_obs_unmixed_left / (4 * num_threads)),
    y_smp_unmixed_left,
    phi_lookup_unmixed_left,
    cdf_lookup_unmixed_left,
    phi,
    unrolled_dose_probs
  );
}
if (n_obs_mixed_left > 0) {
  vector[n_weights_mixed_left] weighted_left =
    weights_mixed_left .*
    (1.0 - phi[phi_lookup_mixed_left]) .*
    unrolled_dose_probs[cdf_lookup_mixed_left];
  target += reduce_sum(
    mixed_left_partial_lcdf,
    y_obs_mixed_left,
    max(1, n_obs_mixed_left / (4 * num_threads)),
    y_smp_mixed_left,
    obs_map_mixed_left,
    weighted_left
  );
}
