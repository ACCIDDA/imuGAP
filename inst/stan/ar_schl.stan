
#include data/definition.stan

parameters { // all of these are on the logit scale
  #include parameters/standard.stan
  #include parameters/cnty_static.stan
  #include parameters/schl_vary.stan
  real<lower=0> sd_schl_ar;
}

transformed parameters {

  #include transforms/shared.stan
  #include transforms/schl_vary.stan
  
  for(i in 1:n_cnty){
    lambda[(cnty_start_index[i]):(cnty_end_index[i]),] += rep_matrix(mu_c[i], school_rep_size[i], n_yr);
  }
  
  #include transforms/lambda_c.stan
  
}

model {
  
  mu ~ normal(logit(0.9), sd_state);
  mu_s[,1] ~ normal(0, sd_schl);
  for (i in 1:n_school) { // for each school, year n+1 is conditioned on n
    mu_s[i,2:] ~ normal(mu_s[i,:(n_yr-1)], sd_schl_ar);
  }
  mu_c ~ normal(0, sd_cnty);
  
  #include model/fitting.stan
  
}
