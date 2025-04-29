
#include data/definition.stan

parameters { // all of these are on the logit scale
  #include parameters/standard.stan
  #include parameters/cnty_static.stan
  #include parameters/schl_static.stan
}

transformed parameters {
  // lambda[schools, years], lambda_c[counties, years]
  #include transforms/shared.stan
  
  // school-level coverage (logit scale)
  for(i in 1:n_cnty){
    lambda[(cnty_start_index[i]):(cnty_end_index[i]),] += rep_matrix(mu_c[i], school_rep_size[i], n_yr);
  }
  
  // lambda_c[index,year]: county-level coverage (logit scale)
  #include transforms/lambda_c.stan
  
}

model {
  
  mu ~ normal(logit(0.9), sd_state);
  mu_c ~ normal(0, sd_cnty);
  mu_s ~ normal(0, sd_schl);

  #include model/fitting.stan
  
}
