/**
 * Stably compute the divided difference (exp(-r1) - exp(-r2)) / (r2 - r1)
 *
 * Used for off-diagonal transition probabilities in continuous-time Markov chains
 * with non-zero interval transitions between dose states.
 *
 * @param delta Difference in cumulative rates (r2 - r1)
 * @param e1    Precomputed exponential decay exp(-r1)
 * @param e2    Precomputed exponential decay exp(-r2)
 *
 * @return Value of (exp(-r1) - exp(-r2)) / (r2 - r1)
 */
real exp_diff_div(real delta, real e1, real e2) {
  if (abs(delta) < 1e-4) {
    // 4th-order Horner form of Taylor expansion around delta = 0 (where limit is exp(-r1))
    return e1 * (1.0 - delta * (0.5 - delta * ((1.0 / 6.0) - delta * ((1.0 / 24.0) - delta * (1.0 / 120.0)))));
  }
  return (e1 - e2) / delta;
}

/**
 * Compute cumulative vaccination dose probabilities across discrete age/time intervals.
 *
 * Simulates a continuous-time Markov chain transition matrix P = exp(Q) for each interval m
 * under piecewise-constant transition hazards lambda_raw.
 *
 * @param n_intervals Number of time/age intervals (M)
 * @param n_doses     Maximum number of doses (D)
 * @param dt_vec      Vector of interval widths in time/age units (length M)
 * @param dose_sched  Binary schedule matrix (M x D), where dose_sched[m, k] = 1 if dose k is active
 * @param lambda_raw  Log-scale transition hazard rates (length D)
 *
 * @return Flattened vector of length M * D containing cumulative dose probabilities (P(Dose >= k))
 */
vector unrolled_dose(int n_intervals, int n_doses, vector dt_vec, matrix dose_sched, vector lambda_raw) {
  int n_states = n_doses + 1;
  vector[n_doses] lambda = exp(lambda_raw);

  // Matrix to store cumulative probabilities: rows = intervals, cols = doses
  matrix[n_intervals, n_doses] conditional_dXcdf;

  // Initial state distribution at t=0: 100% of population in state 1 (0 doses)
  row_vector[n_states] p_state = rep_row_vector(0.0, n_states);
  p_state[1] = 1.0;

  for (m in 1:n_intervals) {
    vector[n_doses] r = (dose_sched[m]' .* lambda) * dt_vec[m];
    vector[n_doses] e = exp(-r);
    row_vector[n_states] p_next;

    if (n_doses <= 3) {
      // Analytical upper-triangular transition matrix P = exp(Q)
      matrix[n_states, n_states] P = rep_matrix(0.0, n_states, n_states);

      for (k in 1:n_doses) {
        P[k, k] = e[k];
      }
      P[n_states, n_states] = 1.0;

      // First sub-diagonal transitions (k -> k + 1)
      if (n_doses >= 2) {
        real diff12 = exp_diff_div(r[2] - r[1], e[1], e[2]);
        P[1, 2] = r[1] * diff12;
        if (n_doses == 3) {
          real diff23 = exp_diff_div(r[3] - r[2], e[2], e[3]);
          P[2, 3] = r[2] * diff23;

          // Second sub-diagonal transition (1 -> 3)
          real d13 = r[3] - r[1];
          if (abs(d13) < 1e-4) {
            real d12 = r[2] - r[1];
            if (abs(d12) < 1e-4) {
              P[1, 3] = 0.5 * r[1] * r[2] * e[1];
            } else {
              P[1, 3] = r[1] * r[2] * (e[1] - diff12) / d12;
            }
          } else {
            P[1, 3] = r[1] * r[2] * (diff12 - diff23) / d13;
          }
        }
      }

      // Absorbing column probabilities via conservation of probability
      for (k in 1:n_doses) {
        P[k, n_states] = 1.0 - sum(P[k, 1:n_doses]);
      }

      p_next = p_state * P;
    } else {
      // General D > 3 fallback using matrix_exp
      matrix[n_states, n_states] Q = rep_matrix(0.0, n_states, n_states);
      for (k in 1:n_doses) {
        Q[k, k]     = -r[k];
        Q[k, k + 1] =  r[k];
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
