
/**
  * Compute cumulative dose coverage at target evaluation times using dynamic
  * matrix exponentials driven by explicit activation thresholds tau.
  *
  * @param eval_ages sorted vector; ages at which to report coverage (e.g., [1.0, 2.0, 3.0])
  * @param tau sorted vector; activation ages by doses (tau_1 < tau_2 < ... < tau_k) for K doses
  * @param lambda vector; same size as tau, the conditional hazards for each dose
  * @return vector (n_doses x n_ages) of cumulative coverage by evaluation time
  */
vector unrolled_dose_tau_vector(vector eval_ages, vector tau, vector lambda) {
  int n_ages = rows(eval_ages);
  int n_doses = rows(tau);
  int n_states = n_doses + 1;
  vector[n_doses] lambda = exp(lambda_raw);
  
  // Allocate single flat vector output: [dose1_t1..tN, dose2_t1..tN, ...]
  vector[n_doses * n_ages] unrolled_cdf;
  
  row_vector[n_states] p_state = rep_row_vector(0.0, n_states);
  p_state[1] = 1.0; // 100% at state 1 (0 doses)
  
  real t_current = 0.0;  

  for (i in 1:n_ages) {
    real t_target = eval_ages[i];
    
    // Step through active thresholds falling in (t_current, t_target)
    for (k in 1:n_doses) {
      if (tau[k] > t_current && tau[k] < t_target) {
        real dt = tau[k] - t_current;
        
        matrix[n_states, n_states] Q = rep_matrix(0.0, n_states, n_states);
        for (d in 1:n_doses) {
          if (t_current >= tau[d]) {
            Q[d, d]     = -lambda[d];
            Q[d, d + 1] =  lambda[d];
          }
        }
        p_state = p_state * matrix_exp(Q * dt);
        t_current = tau[k];
      }
    }
    
    // Advance to evaluation target time
    if (t_target > t_current) {
      real dt = t_target - t_current;
      matrix[n_states, n_states] Q = rep_matrix(0.0, n_states, n_states);
      for (d in 1:n_doses) {
        if (t_current >= tau[d]) {
          Q[d, d]     = -lambda[d];
          Q[d, d + 1] =  lambda[d];
        }
      }
      p_state = p_state * matrix_exp(Q * dt);
      t_current = t_target;
    }
    
    // Flatten directly into unrolled vector offset indexing
    // Mapping: column-major layout (dose d, eval index i) -> (d - 1) * n_ages + i
    for (d in 1:n_doses) {
      int idx = (d - 1) * n_ages + i;
      unrolled_cdf[idx] = sum(p_state[(d + 1):n_states]);
    }
  }
  
  return unrolled_cdf;
}

vector calculate_ps(
  vector eval_ages, vector tau, vector lambda, // terms for unrolled_phi_pdf
  vector phi, vector phi_lookup, vector cdf_lookup, vector weights, // terms for weighted
  int n_obs, array obs_map // map to create p_obs
) {
  int n_doses = rows(tau);
  int n_ages = rows(eval_ages);
  int n_weights = rows(phi);
  vector[n_doses * n_ages] unrolled_dose_probs = unrolled_dose(eval_ages, tau, lambda);
  vector[n_weights] weighted = (1 - phi[phi_lookup]) .* unrolled_dose_probs[cdf_lookup] .* weights;
  vector[n_obs] p_obs;

  for (obs_i in 1:n_obs) {
    p_obs[obs_i] = sum(weighted[obs_map[1,obs_i]:obs_map[2,obs_i]]);
  }
  
  return p_obs;
}
