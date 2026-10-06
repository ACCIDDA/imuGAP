// Log-Exceedance MGF Shift for Logit and Padé [1/1] Resummation for Probit

// -----------------------------------------------------------------------------
// Logit Log-Exceedance / MGF Shift
// -----------------------------------------------------------------------------

/**
 * Computes balanced logit shift using the Moment Generating Function (MGF)
 * / Log-Exceedance identity for extreme upper tails (p0 >= 0.5) and lower tails (p0 < 0.5).
 *
 * @param eta0 Vector of baseline coordinates on logit scale (unused in MGF shift).
 * @param p0 Vector of baseline target probabilities (evaluated at 0.5 split threshold; values in [0, 1]).
 * @param w Vector of child weights summing to 1.
 * @param delta Vector of relative child offsets on logit scale.
 * @return Vector of logit MGF tail shifts.
 */
vector shift_logit_mgf(vector eta0, vector p0, vector w, vector delta) {
  int C = num_elements(p0);
  int K = num_elements(w);
  vector[K] log_w = log(w);

  // Upper tail: mu_upper = log( sum( w .* exp(-delta) ) )
  real mu_upper = log_sum_exp(log_w - delta);
  // Lower tail: mu_lower = -log( sum( w .* exp(delta) ) )
  real mu_lower = -log_sum_exp(log_w + delta);

  vector[C] mu;
  for (c in 1:C) {
    mu[c] = (p0[c] >= 0.5) ? mu_upper : mu_lower;
  }

  return mu;
}

// -----------------------------------------------------------------------------
// Probit Padé [1/1] Rational Resummation
// -----------------------------------------------------------------------------

/**
 * Computes balanced probit shift using a Padé [1/1] rational approximant
 * to damp polynomial Taylor divergence in the tails.
 *
 * @param eta0 Vector of baseline coordinates on probit scale (x0 = inv_Phi(p0)).
 * @param p0 Vector of baseline target probabilities (unused in probit expansion).
 * @param w Vector of child weights summing to 1.
 * @param delta Vector of relative child offsets on probit scale.
 * @return Vector of probit Padé [1/1] shifts.
 */
vector shift_probit_pade11(vector eta0, vector p0, vector w, vector delta) {
  int C = num_elements(eta0);
  real m2 = dot_product(w, square(delta));
  real m3 = dot_product(w, delta .* square(delta));
  real m4 = dot_product(w, square(square(delta)));
  real m2_sq = square(m2);

  vector[C] x0_sq = square(eta0);

  vector[C] mu_2 = 0.5 * m2 * eta0;
  vector[C] mu_3 = -((x0_sq - 1.0) / 6.0) * m3;
  vector[C] mu_4 = (eta0 / 24.0) .* (3.0 * (2.0 - x0_sq) * m2_sq + (x0_sq - 3.0) * m4);

  vector[C] denom = 1.0 - (mu_4 ./ (mu_2 + 1e-12));
  vector[C] mu_sym = mu_2 ./ denom;

  return mu_sym + mu_3;
}
