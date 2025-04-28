
#include /data/definition.stan

parameters { // all of these are on the logit scale
  #include /parameters/standard.stan
  #include /parameters/cnty_vary.stan
  #include /parameters/schl_static.stan
}

transformed parameters {

  #include /transforms/shared.stan
  #include /transforms/schl_static.stan

  for(i in 1:n_cnty) {
    // county level offset changes over time
    lambda[(cnty_start_index[i]):(cnty_end_index[i]),] += rep_matrix(mu_c[i,], school_rep_size[i]);
  }
  
  #include /transforms/lambda_c.stan
  
}

model {
  
  mu ~ normal(logit(0.9), sd_state);
  mu_s ~ normal(0, sd_school);
  // mu_c[,1] ~ normal(0, sd_cnty); don't condition the first offset
  for (i in 1:n_cnty) { // for each county, year n+1 is conditioned on n
    mu_c[i,2:] ~ normal(mu_c[i,:(n_yr-1)], sd_cnty);
  }
  
  #include /model/fitting.stan
  
}

