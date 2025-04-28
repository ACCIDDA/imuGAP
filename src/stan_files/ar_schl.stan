
#include /data/definition.stan

parameters { // all of these are on the logit scale
  #include /parameters/standard.stan
  #include /parameters/county_intercept_static.stan

  matrix[n_school, n_yr] mu_s; // school-level intercepts

}

transformed parameters {
  // lambda[schools, years], lambda_c[counties, years]
  #include /transforms/shared.stan
  #include /transforms/school_vary.stan
  
  for(i in 1:n_cnty){
    lambda[(cnty_start_index[i]):(cnty_end_index[i]),] += rep_matrix(mu_c[i], school_rep_size[i], n_yr);
  }
  
  #include /transforms/lmbda_c.stan
  
}

model {
  
  mu ~ normal(logit(0.9), sd_state);
  mu_s[,1] ~ normal(0, sd_school);
  mu_s[,2:] ~ normal(mu_s[,(n_yr-1)], sd_school)
  mu_c ~ normal(0, sd_cnty);
  
  #include /model/fitting.stan
  
}

