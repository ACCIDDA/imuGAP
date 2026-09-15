real exp_diff_div(real r1, real r2) {
  real delta = r1 - r2;
  if (abs(delta) < 1e-5) {
    return exp(-r1) * (1.0 + delta * (0.5 + delta * ((1.0 / 6.0) + delta * ((1.0 / 24.0) + delta * (1.0 / 120.0)))));
  }
  return exp(-r1) * expm1(delta) / delta;
}

vector unrolled_dose(int n_intervals, int n_doses, vector dt_vec, matrix dose_sched, vector lambda_raw) {
  int n_states = n_doses + 1;
  vector[n_doses] lambda = exp(lambda_raw);

  // Matrix to store cumulative probabilities: rows = intervals, cols = doses
  matrix[n_intervals, n_doses] conditional_dXcdf;

  // Initial state distribution at t=0: 100% of population in state 1 (0 doses)
  row_vector[n_states] p_state = rep_row_vector(0.0, n_states);
  p_state[1] = 1.0;

  for (m in 1:n_intervals) {
    real dt = dt_vec[m];
    row_vector[n_states] p_next = rep_row_vector(0.0, n_states);

    if (n_doses == 1) {
      real r1 = dose_sched[m, 1] * lambda[1] * dt;
      real e1 = exp(-r1);
      p_next[1] = p_state[1] * e1;
      p_next[2] = p_state[2] + p_state[1] * (1.0 - e1);
    } else if (n_doses == 2) {
      real r1 = dose_sched[m, 1] * lambda[1] * dt;
      real r2 = dose_sched[m, 2] * lambda[2] * dt;
      real e1 = exp(-r1);
      real e2 = exp(-r2);

      real p11 = e1;
      real p12;
      if (r1 == 0.0) {
        p12 = 0.0;
      } else if (r2 == 0.0) {
        p12 = 1.0 - e1;
      } else {
        p12 = r1 * exp_diff_div(r1, r2);
      }
      real p13 = 1.0 - p11 - p12;
      real p22 = e2;
      real p23 = 1.0 - e2;

      p_next[1] = p_state[1] * p11;
      p_next[2] = p_state[1] * p12 + p_state[2] * p22;
      p_next[3] = p_state[1] * p13 + p_state[2] * p23 + p_state[3];
    } else if (n_doses == 3) {
      real r1 = dose_sched[m, 1] * lambda[1] * dt;
      real r2 = dose_sched[m, 2] * lambda[2] * dt;
      real r3 = dose_sched[m, 3] * lambda[3] * dt;
      real e1 = exp(-r1);
      real e2 = exp(-r2);
      real e3 = exp(-r3);

      real p11 = e1;
      real p12;
      if (r1 == 0.0) {
        p12 = 0.0;
      } else if (r2 == 0.0) {
        p12 = 1.0 - e1;
      } else {
        p12 = r1 * exp_diff_div(r1, r2);
      }

      real p13;
      if (r1 == 0.0 || r2 == 0.0) {
        p13 = 0.0;
      } else if (r3 == 0.0) {
        p13 = 1.0 - p11 - p12;
      } else {
        real diff12 = exp_diff_div(r1, r2);
        real diff23 = exp_diff_div(r2, r3);
        real d13 = r3 - r1;
        if (abs(d13) < 1e-5) {
          p13 = 0.5 * r1 * r2 * e1;
        } else {
          p13 = r1 * r2 * (diff12 - diff23) / d13;
        }
      }
      real p14 = 1.0 - p11 - p12 - p13;

      real p22 = e2;
      real p23;
      if (r2 == 0.0) {
        p23 = 0.0;
      } else if (r3 == 0.0) {
        p23 = 1.0 - e2;
      } else {
        p23 = r2 * exp_diff_div(r2, r3);
      }
      real p24 = 1.0 - p22 - p23;

      real p33 = e3;
      real p34 = 1.0 - e3;

      p_next[1] = p_state[1] * p11;
      p_next[2] = p_state[1] * p12 + p_state[2] * p22;
      p_next[3] = p_state[1] * p13 + p_state[2] * p23 + p_state[3] * p33;
      p_next[4] = p_state[1] * p14 + p_state[2] * p24 + p_state[3] * p34 + p_state[4];
    } else {
      // General D > 3 fallback using matrix_exp
      matrix[n_states, n_states] Q = rep_matrix(0.0, n_states, n_states);
      for (k in 1:n_doses) {
        real rate = dose_sched[m, k] * lambda[k] * dt;
        Q[k, k]     = -rate;
        Q[k, k + 1] =  rate;
      }
      p_next = p_state * matrix_exp(Q);
    }

    p_state = p_next;

    // Cumulative probability of having received at least dose k by end of interval m
    for (k in 1:n_doses) {
      conditional_dXcdf[m, k] = sum(p_state[(k + 1):n_states]);
    }
  }

  // to_vector() flattens column-by-column:
  // [dose 1 intervals 1..M, dose 2 intervals 1..M, ..., dose D intervals 1..M]
  return to_vector(conditional_dXcdf);
}
