if (n_obs_uncensored > 0) {
  target += binomial_lpmf(y_obs_uncensored | y_smp_uncensored, p_obs_uncensored);
}
