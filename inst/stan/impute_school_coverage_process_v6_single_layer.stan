functions {
  // a function to convert lower bounds l_1, 1_2, ... 1_n
  // to (lower, upper) pairs (l_1, l_2-1), (l_2, l_3-1), ...
  array[,] int bounds_to_range(array[] int lowers, int ub) {
    int size_bounds = size(lowers);
    if (lowers[size_bounds] > ub) {
      print("Upper bound, ", ub, " is less than last lower bound, ",
            lowers[size_bounds]);
    }
    array[size_bounds] int uppers;
    for (i in 1 : (size_bounds - 1)) {
      uppers[i] = lowers[i + 1] - 1;
    }
    uppers[size_bounds] = ub;
    return {lowers, uppers};
  }
  
  // create a matrix, each column multiplied by corresponding row entry
  matrix element_mult_expand(vector colv, row_vector rowv) {
    int nrows = size(colv), ncols = size(rowv);
    matrix[nrows, ncols] result;
    for (i in 1 : nrows) {
      result[i,  : ] = rowv * colv[i];
    }
    return result;
  }
  
  // Sequential diff
  vector diff(vector obj) {
    int sz = size(obj);
    return obj[2 : ] - obj[ : (sz - 1)];
  }
  
  row_vector diff(row_vector obj) {
    int sz = size(obj);
    return obj[2 : ] - obj[ : (sz - 1)];
  }
  
  row_vector colsum(matrix obj) {
    int ncols = cols(obj);
    row_vector[ncols] res;
    for (i in 1 : ncols) {
      res[i] = sum(obj[ : , i]);
    }
    return res;
  }
  
  vector rowsum(matrix obj) {
    int nrows = rows(obj);
    vector[nrows] res;
    for (i in 1 : nrows) {
      res[i] = sum(obj[i,  : ]);
    }
    return res;
  }
  
  vector unrolled_dose(int n_yr, int n_doses, matrix dose_sched,
                       vector lambda_raw, real epsilon_p) {
    // assert: dose_sched is n_yr x n_doses
    // assert: lambda_raw is n_doses x 1 (alt: would be n_doses x n_time)
    vector[n_doses] lambda = exp(lambda_raw);
    
    // the unconditional cdfs, given FoV + whatever remains; normalization factors
    matrix[n_yr, n_doses] dXcdf, dXpdf, normdXpdf;
    vector[n_doses] rem;
    
    for (dose in 1 : n_doses) {
      dXcdf[ : , dose] = 1
                         - exp(-cumulative_sum(dose_sched[ : , dose]
                                               * lambda[dose]));
      rem[dose] = 1 - dXcdf[n_yr, dose];
      dXpdf[1, dose] = dXcdf[1, dose];
      dXpdf[2 : , dose] = diff(dXcdf[ : , dose]);
      normdXpdf[ : , dose] = reverse(cumulative_sum(reverse(dXpdf[ : , dose])))
                             + rem[dose];
    }
    
    matrix[n_yr, n_doses] conditional_dXpdf = rep_matrix(0, n_yr, n_doses), conditional_dXcdf;
    conditional_dXpdf[ : , 1] = dXpdf[ : , 1];
    conditional_dXcdf[ : , 1] = dXcdf[ : , 1];
    
    for (dose in 2 : n_doses) {
      int prev_dose = dose - 1;
      // conditional probability => probability got dose n-1 at some time, then probability got dose n at later times
      for (ly in 1 : n_yr) {
        if (normdXpdf[ly, dose] < epsilon_p) {
          break;
        } // if the remaining weight is negligible, avoid division by zero
        conditional_dXpdf[ly : , dose] += conditional_dXpdf[ly, prev_dose]
                                          * dXpdf[ly : , dose]
                                          / normdXpdf[ly, dose];
      }
      conditional_dXcdf[ : , dose] = cumulative_sum(conditional_dXpdf[ : , dose]);
    }
    
    // TODO check unrolling of cdfs - should be dose 1 all years, then dose 2 all years, etc
    vector[n_doses * n_yr] unrolled_dose_cdf = to_vector(conditional_dXcdf);
    return unrolled_dose_cdf;
  }
}
data {
  // STRUCTURAL DEFINITIONS
  
  int<lower=1> n_yr; // number of years to model for each cohort - should be at least year of oldest observation
  int<lower=1> n_cohort; // number of birth year cohorts
  
  // dose schedules
  int<lower=1> n_doses;
  matrix<lower=0, upper=1>[n_yr, n_doses] dose_sched;
  
  // DATA DEFINITIONS
  
  int<lower=1> n_obs;
  array[n_obs] int<lower=0> y_obs;
  array[n_obs] int<lower=0> y_smp;
  // have school id ranges for observations & for doses; school id 0 == statewide?
  // array[n_obs] int obs_sch_id_bounds;
  
  int<lower=n_obs> n_weights;
  array[n_obs] int<lower=1, upper=n_weights> obs_to_weights_bounds; // each entry is the start of the range
  
  array[n_weights] int<lower=1, upper=n_cohort> weights_cohort;
  array[n_weights] int<lower=1, upper=n_yr> weights_life_year;
  array[n_weights] int<lower=1, upper=n_doses> weights_dose;
  
  vector<lower=0, upper=1>[n_weights] weights; // contribution of this (school, cohort, year, dose) to an observation
  
  // run mode: 0 = estimation, 1 = prediction
  int<lower=0, upper=1> predict_mode;
  
  // TODO: calculate these in stan?
  // https://spinkney.github.io/helpful_stan_functions/group__splines.html
  // state-level basis spline
  int k_bs; // number of bspline basis functions
  matrix[n_cohort, k_bs] bs; // basis functions
  
  // observations may be right-censored
  // observation data is assumed ordered uncensored, then right censored
  // so n_uncensored_obs == n_obs, all observations are uncensored
  // number of uncensored observations
  int<lower=0, upper=n_obs> n_uncensored_obs;
}
transformed data {
  real epsilon_p = 1e-10;
  
  // convert to lookup spans for convenience
  array[2, n_obs] int obs_map = bounds_to_range(obs_to_weights_bounds,
                                                n_weights);
  
  array[n_weights] int<lower=1> phi_lookup;
  array[n_weights] int<lower=1> cdf_lookup;
  // because integer arrays don't support broadcasting ...
  // unroll phi and cdf objects to support vectorization
  for (weight_i in 1 : n_weights) {
    // phi ordered by school then cohort
    phi_lookup[weight_i] = weights_cohort[weight_i];
    // ordered by dose then life year
    cdf_lookup[weight_i] = weights_life_year[weight_i]
                           + (weights_dose[weight_i] - 1) * n_yr;
  }
  
  array[n_obs] int<lower=-1> y_obs_trans;
  
  for (i in 1 : n_obs) {
    y_obs_trans[i] = y_obs[i] - 1;
  }
}
parameters {
  // bases spline coeficcients
  vector[k_bs] beta_bs; // spline betas
  
  // Vaccination uptake rate
  vector[n_doses] lambda_raw;
}
model {
  if (!predict_mode) {
    // PRIORS - spline coefficients
    beta_bs ~ normal(0, 10);
    
    // PRIOR - lambda; relatively strong prior belief that ~95% coverage achieved in a year
    // mean of 3 => 1 - exp(-3*1) == ~ 0.95
    lambda_raw ~ normal(log(3), 1);
    vector[n_obs] p_obs;
    
    vector[n_cohort] logit_phi_st = bs * beta_bs;
    vector[n_cohort] phi = inv_logit(logit_phi_st);
    vector[n_doses * n_yr] unrolled_dose_probs = unrolled_dose(n_yr, n_doses,
                                                               dose_sched,
                                                               lambda_raw,
                                                               epsilon_p);
    vector[n_weights] weighted = (1 - phi[phi_lookup])
                                 .* unrolled_dose_probs[cdf_lookup]
                                 .* weights;
    for (obs_i in 1 : n_obs) {
      p_obs[obs_i] = sum(weighted[obs_map[1, obs_i] : obs_map[2, obs_i]]);
    }
    
    if (n_uncensored_obs < n_obs) {
      // at least some censored observations
      // p_s => 1 - p_s = p_f :: probability of at least this many successes =>
      //                         probability of less than this many failures
      if (n_uncensored_obs > 0) {
        // at least some uncensored observations
        target += binomial_lpmf(y_obs[ : n_uncensored_obs] | y_smp[ : n_uncensored_obs], p_obs[ : n_uncensored_obs]);
      }
      target += binomial_lcdf(y_obs[(n_uncensored_obs + 1) : ] | y_smp[(n_uncensored_obs
                                                                    + 1) : ], 1
                                                                    - p_obs[(n_uncensored_obs
                                                                    + 1) : ]);
    } else {
      // all uncensored observations
      y_obs ~ binomial(y_smp, p_obs); // vectorized
    }
  }
}
generated quantities {
  vector[predict_mode ? n_obs : 0] p_obs;
  if (predict_mode) {
    vector[n_cohort] logit_phi_st = bs * beta_bs;
    vector[n_cohort] phi = inv_logit(logit_phi_st);
    vector[n_doses * n_yr] unrolled_dose_probs = unrolled_dose(n_yr, n_doses,
                                                               dose_sched,
                                                               lambda_raw,
                                                               epsilon_p);
    vector[n_weights] weighted = (1 - phi[phi_lookup])
                                 .* unrolled_dose_probs[cdf_lookup]
                                 .* weights;
    for (obs_i in 1 : n_obs) {
      p_obs[obs_i] = sum(weighted[obs_map[1, obs_i] : obs_map[2, obs_i]]);
    }
  }
}

