
  // state-wide obs => school_id == n_sch + 1; TODO check vectorization flattening order
  vector[n_weights] weighted = (1 - phi[phi_lookup]);
  weighted .*= unrolled_dose_cdf[cdf_lookup];
  weighted .*= weights;

  // convert conditional_dXcdf to p_obs, according to the observation weights
  vector[n_obs] p_obs;
  // the observed probability of having previously gotten dose X is:
  //  - the weighted combination of relevant cohorts X relevant life years X relevant schools
  //  - a cohort X year X school probability is
  //    conditional_dXcdf[dose, cohort_life_year] * (1 - phi[school, cohort])
  for (obs_i in 1:n_obs) {
    p_obs[obs_i] = sum(weighted[obs_map[1,obs_i]:obs_map[2,obs_i]]);
  }
