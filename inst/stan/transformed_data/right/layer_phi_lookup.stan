array[n_obs_unmixed_right] int<lower=1> phi_lookup_unmixed_right =
  compute_phi_lookup(w_cohort_unmixed_right, w_loc_unmixed_right, n_cohort, n_locs);
array[n_weights_mixed_right] int<lower=1> phi_lookup_mixed_right =
  compute_phi_lookup(w_cohort_mixed_right, w_loc_mixed_right, n_cohort, n_locs);
