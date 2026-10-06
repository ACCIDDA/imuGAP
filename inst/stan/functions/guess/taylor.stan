// Analytic Taylor Series Moment-Expansion Solvers for Logit and Probit Links

// -----------------------------------------------------------------------------
// Logit Link Taylor Expansions
// -----------------------------------------------------------------------------

/**
 * 2nd-order Taylor series shift approximation for logit link.
 *
 * @param eta0 Vector of baseline coordinates on logit scale.
 * @param p0 Vector of baseline target probabilities (must be clamped to (0, 1) prior to call).
 * @param w Vector of child weights summing to 1.
 * @param delta Vector of relative child offsets on logit scale.
 * @return Vector of 2nd-order logit link-scale shifts.
 */
vector shift_logit_taylor2(vector eta0, vector p0, vector w, vector delta) {
  real m2 = dot_product(w, square(delta));
  return 0.5 * m2 * (2.0 * p0 - 1.0);
}

/**
 * 4th-order Taylor series shift approximation for logit link.
 *
 * @param eta0 Vector of baseline coordinates on logit scale.
 * @param p0 Vector of baseline target probabilities (must be clamped to (0, 1) prior to call).
 * @param w Vector of child weights summing to 1.
 * @param delta Vector of relative child offsets on logit scale.
 * @return Vector of 4th-order logit link-scale shifts.
 */
vector shift_logit_taylor4(vector eta0, vector p0, vector w, vector delta) {
  int C = num_elements(p0);
  real m2 = dot_product(w, square(delta));
  real m3 = dot_product(w, delta .* square(delta));
  real m4 = dot_product(w, square(square(delta)));
  real m2_sq = square(m2);

  vector[C] v = 2.0 * p0 - 1.0;
  vector[C] p0_sq = square(p0);

  vector[C] mu2 = 0.5 * m2 * v;
  vector[C] mu3 = -((1.0 - 6.0 * p0 + 6.0 * p0_sq) / 6.0) * m3;
  vector[C] term4_m4 = (1.0 - 12.0 * p0 + 12.0 * p0_sq) * m4;
  vector[C] term4_m2 = 3.0 * (1.0 - 4.0 * p0 + 4.0 * p0_sq) * m2_sq;
  vector[C] mu4 = (v / 24.0) .* (term4_m4 - term4_m2);

  return mu2 + mu3 + mu4;
}

// -----------------------------------------------------------------------------
// Probit Link Taylor Expansions
// -----------------------------------------------------------------------------

/**
 * 2nd-order Taylor series shift approximation for probit link.
 *
 * @param eta0 Vector of baseline coordinates on probit scale (x0 = inv_Phi(p0)).
 * @param p0 Vector of baseline target probabilities (unused in probit expansion).
 * @param w Vector of child weights summing to 1.
 * @param delta Vector of relative child offsets on probit scale.
 * @return Vector of 2nd-order probit link-scale shifts.
 */
vector shift_probit_taylor2(vector eta0, vector p0, vector w, vector delta) {
  real m2 = dot_product(w, square(delta));
  return 0.5 * m2 * eta0;
}

/**
 * 4th-order Taylor series shift approximation for probit link.
 *
 * @param eta0 Vector of baseline coordinates on probit scale (x0 = inv_Phi(p0)).
 * @param p0 Vector of baseline target probabilities (unused in probit expansion).
 * @param w Vector of child weights summing to 1.
 * @param delta Vector of relative child offsets on probit scale.
 * @return Vector of 4th-order probit link-scale shifts.
 */
vector shift_probit_taylor4(vector eta0, vector p0, vector w, vector delta) {
  int C = num_elements(eta0);
  real m2 = dot_product(w, square(delta));
  real m3 = dot_product(w, delta .* square(delta));
  real m4 = dot_product(w, square(square(delta)));
  real m2_sq = square(m2);

  vector[C] x0_sq = square(eta0);

  vector[C] mu2 = 0.5 * m2 * eta0;
  vector[C] mu3 = -((x0_sq - 1.0) / 6.0) * m3;
  vector[C] mu4 = (eta0 / 24.0) .* (3.0 * (2.0 - x0_sq) * m2_sq + (x0_sq - 3.0) * m4);

  return mu2 + mu3 + mu4;
}
