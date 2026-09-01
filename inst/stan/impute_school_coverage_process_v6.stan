functions {
  #include functions/convenience.stan
  #include functions/unrolled_dose_static_lambda.stan
}
data {
  #include data/shared.stan
  #include data/bspline.stan
  #include data/censoring.stan
}
transformed data {
  #include transformed_data/epsilon.stan
  #include transformed_data/layer_indices.stan
  #include transformed_data/censoring.stan
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
    vector[n_obs] p_obs;
    #include model/hierarchical_phi.stan
    #include model/censored.stan
  }
}
generated quantities {
  vector[predict_mode ? n_obs : 0] p_obs;
  if (predict_mode) {
    #include model/hierarchical_phi.stan
  }
}
