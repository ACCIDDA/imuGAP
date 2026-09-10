if (n_obs_left > 0) {
  target += binomial_lcdf(y_obs_left | y_smp_left, p_obs_left);
}
