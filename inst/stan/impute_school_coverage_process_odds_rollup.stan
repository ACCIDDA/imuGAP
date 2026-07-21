functions {
  #include functions/convenience.stan
  #include functions/unrolled_dose_static_lambda.stan
}

data {
  #include data/shared.stan
  #include data/shared_odds_weights.stan
  #include data/bspline.stan
  #include data/censoring.stan
}

transformed data {
  #include transformed_data/epsilon.stan
  #include transformed_data/cnty_schl.stan
  #include transformed_data/sum_to_zero_qr.stan
  #include transformed_data/cnty_schl_rollup_weights.stan
  #include transformed_data/censoring.stan
}

parameters {
  #include parameters/bspline.stan
  #include parameters/cnty_sch_sum_to_zero.stan
  #include parameters/static_lambda.stan
}

model {
  if (!predict_mode) {
    #include model/bspline.stan
    #include model/static_lambda.stan
    #include model/cnty_sch_sum_to_zero.stan

    vector[n_obs] p_obs;
    #include model/odds_shared.stan

    #include model/censored.stan
  }
}

generated quantities {
  vector[predict_mode ? n_obs : 0] p_obs;

  if (predict_mode) {
    #include model/odds_shared.stan
  }
}
