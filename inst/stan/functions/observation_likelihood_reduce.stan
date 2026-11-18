/**
 * @file observation_likelihood_reduce.stan
 * @brief Partial sum reduction kernels for within-chain parallel observation likelihood.
 *
 * Implements typed partial sum callbacks compatible with Stan's `reduce_sum`
 * for unmixed and mixed observation streams across uncensored, right-censored,
 * and left-censored likelihoods.
 */

/**
 * Numerically stable log binomial CDF, log P(Y <= y | n, p).
 *
 * Stan's built-in `binomial_lcdf` returns non-finite gradients for large `n`
 * (observed from n ~ 2000 upward, even with p == y / n), which makes the
 * sampler fail to initialize. Below `n_exact_max` we use the exact
 * `binomial_lcdf`; at or above it we use a continuity-corrected normal
 * approximation through `std_normal_lcdf`, which is accurate near the bulk of
 * the distribution (where the posterior lives for large n) and only diverges
 * in the far tails.
 */
real stable_binom_log_cdf(int y, int n, real p) {
  int n_exact_max = 1500;
  if (y >= n) {
    return 0.0;
  }
  if (n < n_exact_max) {
    return binomial_lcdf(y | n, p);
  }
  real mu = n * p;
  real sd = sqrt(fmax(n * p * (1.0 - p), 1e-12));
  return std_normal_lcdf((y + 0.5 - mu) / sd);
}

/**
 * Sum of `stable_binom_log_cdf` over paired arrays.
 */
real stable_binom_log_cdf_sum(array[] int y, array[] int n, vector p) {
  real lp = 0.0;
  for (i in 1:size(y)) {
    lp += stable_binom_log_cdf(y[i], n[i], p[i]);
  }
  return lp;
}

/**
 * Partial log-likelihood for unmixed uncensored observations (unnormalized).
 */
real unmixed_uncensored_partial_lpmf(
  array[] int y_obs_slice,
  int start,
  int end,
  array[] int y_smp,
  vector p
) {
  return binomial_lupmf(y_obs_slice | y_smp[start:end], p[start:end]);
}

/**
 * Partial log-likelihood for mixed uncensored observations (unnormalized).
 */
real mixed_uncensored_partial_lpmf(
  array[] int y_obs_slice,
  int start,
  int end,
  array[] int y_smp,
  array[,] int obs_map,
  vector weighted
) {
  real lp = 0.0;
  int len = end - start + 1;
  for (i in 1:len) {
    int global_idx = start + i - 1;
    int st = obs_map[1, global_idx];
    int en = obs_map[2, global_idx];
    real p_i = sum(segment(weighted, st, en - st + 1));
    lp += binomial_lupmf(y_obs_slice[i] | y_smp[global_idx], p_i);
  }
  return lp;
}

/**
 * Partial log-likelihood for unmixed right-censored observations.
 */
real unmixed_right_partial_lcdf(
  array[] int y_fail_slice,
  int start,
  int end,
  array[] int y_smp,
  array[] int phi_lookup,
  array[] int cdf_lookup,
  vector phi,
  vector unrolled_dose_probs
) {
  int len = end - start + 1;
  vector[len] p =
    (1.0 - phi[phi_lookup[start:end]]) .* unrolled_dose_probs[cdf_lookup[start:end]];
  return stable_binom_log_cdf_sum(y_fail_slice, y_smp[start:end], 1.0 - p);
}

/**
 * Partial log-likelihood for mixed right-censored observations.
 */
real mixed_right_partial_lcdf(
  array[] int y_fail_slice,
  int start,
  int end,
  array[] int y_smp,
  array[,] int obs_map,
  vector weighted
) {
  real lp = 0.0;
  int len = end - start + 1;
  for (i in 1:len) {
    int global_idx = start + i - 1;
    int st = obs_map[1, global_idx];
    int en = obs_map[2, global_idx];
    real p_i = sum(segment(weighted, st, en - st + 1));
    lp += stable_binom_log_cdf(y_fail_slice[i], y_smp[global_idx], 1.0 - p_i);
  }
  return lp;
}

/**
 * Partial log-likelihood for unmixed left-censored observations.
 */
real unmixed_left_partial_lcdf(
  array[] int y_obs_slice,
  int start,
  int end,
  array[] int y_smp,
  array[] int phi_lookup,
  array[] int cdf_lookup,
  vector phi,
  vector unrolled_dose_probs
) {
  int len = end - start + 1;
  vector[len] p =
    (1.0 - phi[phi_lookup[start:end]]) .* unrolled_dose_probs[cdf_lookup[start:end]];
  return stable_binom_log_cdf_sum(y_obs_slice, y_smp[start:end], p);
}

/**
 * Partial log-likelihood for mixed left-censored observations.
 */
real mixed_left_partial_lcdf(
  array[] int y_obs_slice,
  int start,
  int end,
  array[] int y_smp,
  array[,] int obs_map,
  vector weighted
) {
  real lp = 0.0;
  int len = end - start + 1;
  for (i in 1:len) {
    int global_idx = start + i - 1;
    int st = obs_map[1, global_idx];
    int en = obs_map[2, global_idx];
    real p_i = sum(segment(weighted, st, en - st + 1));
    lp += stable_binom_log_cdf(y_obs_slice[i], y_smp[global_idx], p_i);
  }
  return lp;
}
