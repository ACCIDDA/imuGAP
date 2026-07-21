    // Offsets priors
    sigma_cnty ~ cauchy(0, 1);
    off_cnty_raw ~ normal(0, sigma_cnty);

    sigma_sch ~ cauchy(0, 2);
    off_sch_raw ~ normal(0, sigma_sch);
