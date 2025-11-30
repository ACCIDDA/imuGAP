
    // Offsets - non-centered parameterization to speed up
    sigma_cnty ~ cauchy(0, 1);
    off_cnty ~ normal(0, sigma_cnty);

    sigma_sch ~ cauchy(0, 2);
    off_sch ~ normal(0, sigma_sch);
