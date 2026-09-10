if (n_obs_right > 0) {
  target += binomial_lcdf(y_fail_right | y_smp_right, 1 - p_obs_right);
}
