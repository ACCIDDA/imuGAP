
data {
  int n_yr; // number of years (grade cohorts) 
  int n_cnty; // number of counties
  int n_school; // number of counties
  int n_state_obs; // number of state-level coverage observations
  int n_sch_obs; // number of school-level coverage observations
  
  // Defines chunks of schools in the same county
  int cnty_start_index[n_cnty];
  int cnty_end_index[n_cnty];
  
  // weights
  matrix[n_school, n_yr] sch_wts; // school weights = enrollment at school/total enrollment in county
  matrix[n_cnty, n_yr] pop_wts[3]; // county population weights = county pop for age group/state pop for age group for 19-35 mos (1), kindergarten entry (2), and 13-15 yos (3)
  
  // school-level observations
  int y_sch[n_sch_obs, 2]; // observed school-level num up to date (col 1) and total enrollment (col 2)
  int y_sch_id[n_sch_obs]; // school IDs
  int y_sch_yr[n_sch_obs]; // years
  
  // state-level observations
  int y_state[n_state_obs, 2]; // state-level coverage observations num up to date (col 1) and total enrollment (col 2)
  matrix[n_state_obs, n_yr] y_state_grades; // weights for grade cohorts corresponding to state-level observations
  int y_state_pop[n_state_obs]; // 19-35 mos (1), kindergarten entry (2), and 13-15 yos (3)
}


parameters { // all of these are on the logit scale
  row_vector[n_yr] mu; // time-varying state-level intercept
  matrix[n_cnty, n_yr] mu_c; // county-level intercepts
  vector[n_school] mu_s; // school-level intercepts
  real dose2; // odds of compliance with second dose after receiving first dose
  real<lower=0> sd_school;
  real<lower=0> sd_cnty;
  real<lower=0> sd_cnty_ar[n_cnty];
  real<lower=0> sd_state;
  real<lower=0> sd_state_ar;
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
  
  sd_cnty_ar ~ inv_gamma(4,1);
  sd_cnty ~ inv_gamma(6,0.5);
  sd_state ~ inv_gamma(16,12);
  sd_state_ar ~ inv_gamma(8,3);
  sd_school ~ inv_gamma(16,12);
  
  mu[1] ~ normal(logit(0.9), sd_state);
  for(i in 2:n_yr) {
    mu[i] ~ normal(mu[i-1], sd_state_ar);
  }
  mu_s ~ normal(0, sd_school);
  mu_c[,1] ~ normal(0, sd_cnty);
  for(i in 1:n_cnty) {
    mu_c[i, 2:] ~ normal(mu_c[i,:(n_yr-1)], sd_cnty_ar[i]);
  }
  
  // school-level observations
  for(i in 1:n_sch_obs) {
    y_sch[i, 1] ~ binomial(y_sch[i, 2], inv_logit(lambda[y_sch_id[i], y_sch_yr[i]]));
  }
  
  // state-level observations
  for(i in 1:n_state_obs) {
    vector[n_yr] lambda_s; // state-level coverages for each year for the given age group
    real dose_adj;
    
    if(y_state_pop[i] == 1) {
      dose_adj = dose2;
    } else {
      dose_adj = 0;
    }
    
    lambda_s = diagonal((lambda_c')*pop_wts[y_state_pop[i]]);
    y_state[i, 1] ~ binomial(y_state[i, 2], inv_logit((y_state_grades[i,]*lambda_s)/sum(y_state_grades[i,])+dose_adj));
    
  }
  
}

