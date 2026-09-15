if (n_obs_unmixed_uncensored > 0) {
  target += binomial_lpmf(y_obs_unmixed_uncensored | y_smp_unmixed_uncensored, p_obs_unmixed_uncensored);
}
if (n_obs_mixed_uncensored > 0) {
  target += binomial_lpmf(y_obs_mixed_uncensored | y_smp_mixed_uncensored, p_obs_mixed_uncensored);
}
