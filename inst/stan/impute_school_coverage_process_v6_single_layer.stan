functions {
  #include functions/convenience.stan
  #include functions/unrolled_dose_static_lambda.stan
}
data {
  #include data/shared.stan
  #include data/bspline.stan
}
transformed data {
  #include transformed_data/epsilon.stan
  #include transformed_data/common_indices.stan
  #include transformed_data/right/observations.stan
  #include transformed_data/single_phi_lookup.stan
}
parameters {
  #include parameters/bspline.stan
  #include parameters/static_lambda.stan
}
model {
  if (!predict_mode) {
    #include model/bspline.stan
    #include model/static_lambda.stan
    #include model/single_phi.stan
    #include model/observation_likelihood.stan
  }
}
generated quantities {
  vector[predict_mode ? n_obs_uncensored : 0] p_obs;
  if (predict_mode) {
    #include model/single_phi.stan
    p_obs = p_obs_uncensored;
  }
}
