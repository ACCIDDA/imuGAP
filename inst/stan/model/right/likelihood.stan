if (n_obs_unmixed_right > 0) {
  target += binomial_lcdf(y_fail_unmixed_right | y_smp_unmixed_right, 1 - p_obs_unmixed_right);
}
if (n_obs_mixed_right > 0) {
  target += binomial_lcdf(y_fail_mixed_right | y_smp_mixed_right, 1 - p_obs_mixed_right);
}
