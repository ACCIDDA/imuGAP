
if (n_censor_obs) { # at least some censored observations
    # p_s => 1 - p_s = p_f :: probability of at least this many successes =>
    #                         probability of less than this many failures 
    target += binomial_lcdf(y_obs[:n_censor_obs] | y_smp[:n_censor_obs], 1 - p_obs[:n_censor_obs]);
    if (n_censor_obs < n_obs) { # at least some uncensored observations
        target += binomial_lpmf(y_obs[(n_censor_obs + 1):] | y_smp[(n_censor_obs + 1):], p_obs[(n_censor_obs + 1):]);
    }
} else { # all uncensored observations
    y_obs ~ binomial(y_smp, p_obs); # vectorized
}
