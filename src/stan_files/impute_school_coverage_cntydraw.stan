
#include /data/definition.stan

parameters { // all of these are on the logit scale
  #include /parameters/standard.stan
  #include /parameters/county_intercept_static.stan
  #include /parameters/school_intercept_static.stan

  matrix[n_cnty, n_yr] mu_c_draw; // county-level intercepts
  real<lower=0> sd_cnty_draw[n_cnty];
}

transformed parameters {
  // lambda[schools, years], lambda_c[counties, years]
  #include /tranforms/lambda.stan
  
  // school-level coverage (logit scale)
  for(i in 1:n_cnty){
    lambda[(cnty_start_index[i]):(cnty_end_index[i]),] = 
      #include /tranforms/broadcast_state.stan
      + 
      rep_matrix(mu_c_draw[i,], cnty_end_index[i] - cnty_start_index[i] + 1)  +
      #include /tranforms/broadcast_school.stan
      ;
  }
  
  // lambda_c[index,year]: county-level coverage (logit scale)
  #include /tranforms/county_coverage.stan
  
}

model {
  
  sd_cnty_draw ~ inv_gamma(5,1);
  
  mu ~ normal(logit(0.9), sd_state);
  mu_s ~ normal(0, sd_school);
  mu_c ~ normal(0, sd_cnty);
  
  //
  for(i in 1:n_cnty) {
    mu_c_draw[i,] ~ normal(mu_c[i], sd_cnty_draw[i]);
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

