/**
 * @file pointwise_log_lik.stan
 * @brief Pointwise observation log-likelihood extraction for PSIS-LOO and WAIC.
 *
 * Populates vector log_lik in consistent row order across observation streams:
 *   1. Unmixed uncensored   (binomial_lpmf)
 *   2. Mixed uncensored     (binomial_lpmf on mixture probability)
 *   3. Unmixed right-cens   (binomial_lcdf of survival failure)
 *   4. Mixed right-cens     (binomial_lcdf of mixture survival failure)
 *   5. Unmixed left-cens    (binomial_lcdf of event occurrence)
 *   6. Mixed left-cens      (binomial_lcdf of mixture event occurrence)
 */
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
