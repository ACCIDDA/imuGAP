array[n_obs_unmixed_uncensored] int<lower=1> phi_lookup_unmixed_uncensored =
  compute_phi_lookup(w_cohort_unmixed_uncensored, w_loc_unmixed_uncensored, n_cohort, n_locs);
array[n_weights_mixed_uncensored] int<lower=1> phi_lookup_mixed_uncensored =
  compute_phi_lookup(w_cohort_mixed_uncensored, w_loc_mixed_uncensored, n_cohort, n_locs);
