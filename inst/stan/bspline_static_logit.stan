functions {
  #include functions/convenience.stan
  #include functions/link/logit.stan
  #include functions/layer_offsets.stan
  #include functions/unrolled_dose_static_lambda.stan
  #include functions/observation_likelihood_reduce.stan
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
transformed parameters {
  #include transformed_parameters/bspline.stan
  #include transformed_parameters/layer_offsets.stan
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
  vector[predict_mode ? n_obs_unmixed_uncensored : 0] p_obs;
  vector[compute_log_lik ? (n_obs_unmixed_uncensored + n_obs_mixed_uncensored + n_obs_unmixed_right + n_obs_mixed_right + n_obs_unmixed_left + n_obs_mixed_left) : 0] log_lik;
  if (predict_mode || compute_log_lik) {
    #include model/hierarchical_phi.stan
    if (predict_mode) {
      p_obs = p_obs_unmixed_uncensored;
    }
    if (compute_log_lik) {
      #include generated_quantities/pointwise_log_lik.stan
    }
  }
}
