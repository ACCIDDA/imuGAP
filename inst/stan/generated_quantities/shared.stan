
vector<lower=0, upper=1>[n_doses * n_yr] unrolled_dose_probs_gen = unrolled_dose(n_yr, n_doses, dose_sched, lambda_raw, epsilon_p);


// state-wide obs => school_id == n_sch + 1; TODO check vectorization flattening order
vector<lower=0,upper=1>[n_weights] weighted_gen = (1 - phi[phi_lookup]) .* unrolled_dose_probs[cdf_lookup] .* weights;

// convert conditional_dXcdf to p_obs, according to the observation weights
vector<lower=0,upper=1>[n_obs] p_gen;
// the observed probability of having previously gotten dose X is:
//  - the weighted combination of relevant cohorts X relevant life years X relevant schools
//  - a cohort X year X school probability is
//    conditional_dXcdf[dose, cohort_life_year] * (1 - phi[school, cohort])
for (obs_i in 1:n_obs) {
  p_gen[obs_i] = sum(weighted[obs_map[1,obs_i]:obs_map[2,obs_i]]);
}
