
    // PRIOR - lambda; relatively strong prior belief that ~95% coverage achieved in a year
    // mean of 3 => 1 - exp(-3*1) == ~ 0.95
    lambda_raw ~ normal(log(3), 1);
