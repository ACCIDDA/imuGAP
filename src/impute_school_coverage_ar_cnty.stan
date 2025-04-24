
#include data_def.stan

parameters { // all of these are on the logit scale
  row_vector[n_yr] mu; // time-varying state-level intercept
  matrix[n_cnty, n_yr] mu_c; // county-level intercepts
  vector[n_school] mu_s; // school-level intercepts
  real compliance; // odds of compliance with second dose after receiving first dose
  real<lower=0> sd_school;
  real<lower=0> sd_cnty;
  real<lower=0> sd_state;
}

transformed parameters {
  matrix[n_school, n_yr] lambda; // school-level coverage
  matrix[n_cnty, n_yr] lambda_c; // county-level coverage
  
  // school-level coverage (logit scale)
  for(i in 1:n_cnty){
    lambda[(cnty_start_index[i]):(cnty_end_index[i]),] = 
      rep_matrix(mu, cnty_end_index[i] - cnty_start_index[i] + 1) + 
      rep_matrix(mu_c[i,], cnty_end_index[i] - cnty_start_index[i] + 1)  +
      rep_matrix(mu_s[(cnty_start_index[i]):(cnty_end_index[i])], n_yr);
  }
  
  // county-level coverage (logit scale)
  for(i in 1:n_cnty){
    lambda_c[i,] = diagonal((lambda[(cnty_start_index[i]):(cnty_end_index[i]),]')*sch_wts[(cnty_start_index[i]):(cnty_end_index[i])])';
  }
  
}

model {
  
  mu ~ normal(logit(0.9), sd_state);
  mu_s ~ normal(0, sd_school);
  mu_c[,1] ~ normal(0, sd_cnty);
  for(i in 2:(n_yr - 1)) {
    mu_c[,i] ~ normal(mu_c[,i-1], sd_cnty);
  }
  
  // school-level observations
  for(i in 1:n_sch_obs) {
    y_sch[i, 1] ~ binomial(y_sch[i, 2], inv_logit(lambda[y_sch_id[i], y_sch_yr[i]]));
  }
  
  // state-level observations
  for(i in 1:n_state_obs) {
    vector[n_yr] lambda_s; // state-level coverages for each year for the given age group
    
    lambda_s = diagonal((lambda_c')*pop_wts[y_state_pop[i]]);
    y_state[i, 1] ~ binomial(y_state[i, 2], inv_logit((y_state_grades[i,]*lambda_s)/sum(y_state_grades[i,])));
    
  }
  
}

