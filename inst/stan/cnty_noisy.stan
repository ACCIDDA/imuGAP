
#include data/definition.stan

parameters { // all of these are on the logit scale
  #include parameters/standard.stan
  #include parameters/cnty_static.stan
  #include parameters/schl_static.stan

  matrix[n_cnty, n_yr] mu_c_draw; // county-level intercepts
  real<lower=0> sd_cnty_draw[n_cnty];
}

transformed parameters {

  #include transforms/shared.stan
  
  // school-level coverage (logit scale)
  for(i in 1:n_cnty){
    lambda[(cnty_start_index[i]):(cnty_end_index[i]),] += rep_matrix(mu_c_draw[i,], school_rep_size[i]);
  }
  
  // lambda_c[index,year]: county-level coverage (logit scale)
  #include transforms/lambda_c.stan
  
}

model {
  
  sd_cnty_draw ~ inv_gamma(5,1);
  
  mu ~ normal(logit(0.9), sd_state);
  mu_s ~ normal(0, sd_schl);
  mu_c ~ normal(0, sd_cnty);
  
  for(i in 1:n_cnty) {
    mu_c_draw[i,] ~ normal(mu_c[i], sd_cnty_draw[i]);
  }
  
  #include model/fitting.stan
  
}
