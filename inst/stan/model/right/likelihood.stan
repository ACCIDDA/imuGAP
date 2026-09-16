if (n_obs_unmixed_right > 0) {
  target += reduce_sum(
    unmixed_right_partial_lcdf,
    y_fail_unmixed_right,
    max(1, n_obs_unmixed_right / (4 * num_threads)),
    y_smp_unmixed_right,
    phi_lookup_unmixed_right,
    cdf_lookup_unmixed_right,
    phi,
    unrolled_dose_probs
  );
}
if (n_obs_mixed_right > 0) {
  vector[n_weights_mixed_right] weighted_right =
    weights_mixed_right .*
    (1.0 - phi[phi_lookup_mixed_right]) .*
    unrolled_dose_probs[cdf_lookup_mixed_right];
  target += reduce_sum(
    mixed_right_partial_lcdf,
    y_fail_mixed_right,
    max(1, n_obs_mixed_right / (4 * num_threads)),
    y_smp_mixed_right,
    obs_map_mixed_right,
    weighted_right
  );
}
