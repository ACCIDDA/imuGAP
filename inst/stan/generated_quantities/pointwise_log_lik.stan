{
  int ll_offset = 0;
  if (n_obs_unmixed_uncensored > 0) {
    for (i in 1:n_obs_unmixed_uncensored) {
      log_lik[ll_offset + i] = binomial_lpmf(
        y_obs_unmixed_uncensored[i] | y_smp_unmixed_uncensored[i], p_obs_unmixed_uncensored[i]
      );
    }
    ll_offset += n_obs_unmixed_uncensored;
  }
  if (n_obs_mixed_uncensored > 0) {
    for (i in 1:n_obs_mixed_uncensored) {
      log_lik[ll_offset + i] = binomial_lpmf(
        y_obs_mixed_uncensored[i] | y_smp_mixed_uncensored[i], p_obs_mixed_uncensored[i]
      );
    }
    ll_offset += n_obs_mixed_uncensored;
  }
  if (n_obs_unmixed_right > 0) {
    for (i in 1:n_obs_unmixed_right) {
      log_lik[ll_offset + i] = binomial_lcdf(
        y_fail_unmixed_right[i] | y_smp_unmixed_right[i], 1.0 - p_obs_unmixed_right[i]
      );
    }
    ll_offset += n_obs_unmixed_right;
  }
  if (n_obs_mixed_right > 0) {
    for (i in 1:n_obs_mixed_right) {
      log_lik[ll_offset + i] = binomial_lcdf(
        y_fail_mixed_right[i] | y_smp_mixed_right[i], 1.0 - p_obs_mixed_right[i]
      );
    }
    ll_offset += n_obs_mixed_right;
  }
  if (n_obs_unmixed_left > 0) {
    for (i in 1:n_obs_unmixed_left) {
      log_lik[ll_offset + i] = binomial_lcdf(
        y_obs_unmixed_left[i] | y_smp_unmixed_left[i], p_obs_unmixed_left[i]
      );
    }
    ll_offset += n_obs_unmixed_left;
  }
  if (n_obs_mixed_left > 0) {
    for (i in 1:n_obs_mixed_left) {
      log_lik[ll_offset + i] = binomial_lcdf(
        y_obs_mixed_left[i] | y_smp_mixed_left[i], p_obs_mixed_left[i]
      );
    }
    ll_offset += n_obs_mixed_left;
  }
}
