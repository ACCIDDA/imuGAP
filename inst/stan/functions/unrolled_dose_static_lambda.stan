vector unrolled_dose(int n_yr, int n_doses, matrix dose_sched, vector lambda_raw) {
  int n_states = n_doses + 1;
  vector[n_doses] lambda = exp(lambda_raw);

  // Matrix to store cumulative probabilities: rows = years, cols = doses
  matrix[n_yr, n_doses] conditional_dXcdf;

  // Initial state distribution at t=0: 100% of population in state 1 (0 doses)
  row_vector[n_states] p_state = rep_row_vector(0.0, n_states);
  p_state[1] = 1.0;

  for (y in 1:n_yr) {
    // 1. Construct the transition rate matrix Q for year y
    matrix[n_states, n_states] Q = rep_matrix(0.0, n_states, n_states);

    for (k in 1:n_doses) {
      // Effective hazard for dose k in year y
      real rate = dose_sched[y, k] * lambda[k];
      Q[k, k]     = -rate; // Outflow from state k
      Q[k, k + 1] =  rate; // Inflow to state k+1
    }
    // Note: State n_states (n_doses + 1) is absorbing, so row n_states remains all 0.0

    // 2. Propagate state probabilities over 1 unit of time (dt = 1 year)
    // p_state(t + 1) = p_state(t) * exp(Q * 1.0)
    p_state = p_state * matrix_exp(Q);

    // 3. Compute cumulative probability of having received at least dose k by end of year y.
    // Having received >= k doses corresponds to being in state (k+1) or higher.
    for (k in 1:n_doses) {
      conditional_dXcdf[y, k] = sum(p_state[(k + 1):n_states]);
    }
  }

  // to_vector() flattens column-by-column:
  // [dose 1 year 1..N, dose 2 year 1..N, ..., dose D year 1..N]
  return to_vector(conditional_dXcdf);
}
