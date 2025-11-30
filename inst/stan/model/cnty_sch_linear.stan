
    // offsets - as normal
    // TODO, figure out how to: #include "./model/cnty_sch.stan"

    sigma_cnty ~ cauchy(0, 1);
    off_cnty ~ normal(0, sigma_cnty);

    sigma_sch ~ cauchy(0, 2);
    off_sch ~ normal(0, sigma_sch);

    sigma_trend_cnty ~ cauchy(0, 0.5);
    trend_cnty ~ normal(0, sigma_trend_cnty);

    sigma_trend_sch ~ cauchy(0, 1);
    trend_sch ~ normal(0, sigma_trend_sch);
