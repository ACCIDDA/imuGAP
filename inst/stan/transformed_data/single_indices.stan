  #include transformed_data/common_indices.stan

  array[n_weights_uncensored] int<lower=1> phi_lookup_uncensored = weights_cohort_uncensored;
  array[n_weights_right] int<lower=1> phi_lookup_right = weights_cohort_right;
  array[n_weights_left] int<lower=1> phi_lookup_left = weights_cohort_left;
