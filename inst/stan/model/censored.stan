
// all observations are provided directly as vaccinations (y_obs_*) out of eligible (y_smp_*)
// the model supports three observation types:
//  - y_(obs|smp)_uncensored: observations where vaccinee count for target vaccine is this value
//  - y_(obs|smp)_right: ... count is at least this value
//  - y_(obs|smp)_left: ... count is at most this value

if (n_obs_uncensored > 0) {
  target += binomial_lpmf(y_obs_uncensored | y_smp_uncensored, p_obs_uncensored);
}
if (n_obs_left > 0) {
  target += binomial_lcdf(y_obs_left | y_smp_left, p_obs_left);
}
if (n_obs_right > 0) {
  target += binomial_lcdf(y_fail_right | y_smp_right, 1 - p_obs_right);
}

