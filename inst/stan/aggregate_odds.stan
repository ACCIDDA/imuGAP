#include functions/collection.stan

data {
  #include data/shared.stan
  #include data/size_balance.stan
  #include data/bspline.stan
  #include data/censoring.stan
}

transformed data {
  #include transformed_data/observation_map.stan
  #include transformed_data/layer_offsets_constrained.stan
}

parameters {
  #include parameters/bspline.stan
  #include parameters/layer_offsets.stan
  #include parameters/static_lambda.stan
}

model {
  if (!predict_mode) {
    // setup this models parameters
    #include model/bspline.stan
    #include model/static_lambda.stan
    #include model/cnty_sch.stan

    // do this models calculations
    #include model/odds_shared.stan

    // create the probabilities for this model
    #include model/p_obs.stan

    // evaluate likelihoods
    #include model/censored.stan
  }
}

generated quantities {
  vector[predict_mode ? n_obs : 0] p_obs;

  if (predict_mode) {
    #include model/odds_shared.stan
  }
}
