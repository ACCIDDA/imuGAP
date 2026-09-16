if (n_obs_unmixed_uncensored > 0) {
  vector[n_obs_unmixed_uncensored] p_unmixed_uncensored =
    (1.0 - phi[phi_lookup_unmixed_uncensored]) .*
    unrolled_dose_probs[cdf_lookup_unmixed_uncensored];
  target += reduce_sum(
    unmixed_uncensored_partial_lupmf,
    y_obs_unmixed_uncensored,
    max(1, n_obs_unmixed_uncensored / (4 * num_threads)),
    y_smp_unmixed_uncensored,
    p_unmixed_uncensored
  );
}
if (n_obs_mixed_uncensored > 0) {
  vector[n_weights_mixed_uncensored] weighted_uncensored =
    weights_mixed_uncensored .*
    (1.0 - phi[phi_lookup_mixed_uncensored]) .*
    unrolled_dose_probs[cdf_lookup_mixed_uncensored];
  target += reduce_sum(
    mixed_uncensored_partial_lupmf,
    y_obs_mixed_uncensored,
    max(1, n_obs_mixed_uncensored / (4 * num_threads)),
    y_smp_mixed_uncensored,
    obs_map_mixed_uncensored,
    weighted_uncensored
  );
}
