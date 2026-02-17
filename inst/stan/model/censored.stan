
if (n_censor_obs) { # at least some censored observations

    for(i in 1:n_censor_obs) {
      if(y_obs_trans[i] >= 0) {
        target += binomial_lccdf(y_obs_trans[i] | y_smp[i], p_obs[i]);
      }
    }
    if (n_censor_obs < n_obs) { # at least some uncensored observations
        target += binomial_lpmf(y_obs[(n_censor_obs + 1):n_obs] | y_smp[(n_censor_obs + 1):n_obs], p_obs[(n_censor_obs + 1):n_obs]);
    }
} else { # all uncensored observations
    y_obs ~ binomial(y_smp, p_obs); # vectorized
}
