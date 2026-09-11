functions {
  #include functions/convenience.stan
  #include functions/layer_offsets.stan
  #include functions/unrolled_dose_static_lambda.stan
}
data {
  #include data/shared.stan
  #include data/locations.stan
  #include data/uncensored/weights_location.stan
  #include data/right/weights_location.stan
  #include data/left/weights_location.stan
  #include data/bspline.stan
}
transformed data {
  #include transformed_data/common_indices.stan
  #include transformed_data/right/observations.stan
  #include transformed_data/layer_indices.stan
  #include transformed_data/layer_phi_lookup.stan
}
parameters {
  #include parameters/bspline.stan
  #include parameters/layer_offsets.stan
  #include parameters/static_lambda.stan
}
model {
  if (!predict_mode) {
    #include model/bspline.stan
    #include model/static_lambda.stan
    #include model/layer_offsets.stan
    #include model/hierarchical_phi.stan
    #include model/observation_likelihood.stan
  }
}
generated quantities {
  vector[predict_mode ? n_obs_uncensored : 0] p_obs;
  if (predict_mode) {
    #include model/hierarchical_phi.stan
    p_obs = p_obs_uncensored;
  }
}
