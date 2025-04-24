  row_vector[n_yr] mu; // time-varying state-level intercept
  real<lower=0> sd_school;
  real<lower=0> sd_cnty;
  real<lower=0> sd_state;
  real compliance; // odds of compliance with second dose after receiving first dose