if (n_obs_unmixed_left > 0) {
  target += binomial_lcdf(y_obs_unmixed_left | y_smp_unmixed_left, p_obs_unmixed_left);
}
if (n_obs_mixed_left > 0) {
  target += binomial_lcdf(y_obs_mixed_left | y_smp_mixed_left, p_obs_mixed_left);
}
