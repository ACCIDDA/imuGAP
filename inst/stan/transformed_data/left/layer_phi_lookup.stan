array[n_obs_unmixed_left] int<lower=1> phi_lookup_unmixed_left =
  compute_phi_lookup(w_cohort_unmixed_left, w_loc_unmixed_left, n_cohort, n_locs);
array[n_weights_mixed_left] int<lower=1> phi_lookup_mixed_left =
  compute_phi_lookup(w_cohort_mixed_left, w_loc_mixed_left, n_cohort, n_locs);
