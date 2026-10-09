/**
 * @file taylor_probit.stan
 * @brief Taylor series shift guess implementations for probit link.
 */

/**
 * Approximate probit link aggregation shift via 2nd-order Taylor expansion.
 *
 * @param eta0 Vector of baseline parent coordinates on probit scale (length C).
 * @param p0 Vector of baseline target probabilities (length C).
 * @param w Vector of child weights summing to 1 (length K).
 * @param delta Vector of relative child offsets on probit scale (length K).
 * @return Vector of 2nd-order shift approximations mu (length C).
 */
vector guess_shift_taylor2_probit(vector eta0, vector p0, data vector w, vector delta) {
  real m2 = dot_product(w, square(delta));
  return 0.5 * m2 * eta0;
}

/**
 * Approximate probit link aggregation shift via 4th-order Taylor expansion.
 *
 * @param eta0 Vector of baseline parent coordinates on probit scale (length C).
 * @param p0 Vector of baseline target probabilities (length C).
 * @param w Vector of child weights summing to 1 (length K).
 * @param delta Vector of relative child offsets on probit scale (length K).
 * @return Vector of 4th-order shift approximations mu (length C).
 */
vector guess_shift_taylor4_probit(vector eta0, vector p0, data vector w, vector delta) {
  int C = num_elements(eta0);
  int K = num_elements(w);
  vector[K] delta2 = square(delta);
  real m2 = dot_product(w, delta2);
  real m3 = dot_product(w, delta .* delta2);
  real m4 = dot_product(w, square(delta2));
  real m2_sq = square(m2);

  vector[C] x0_sq = square(eta0);

  vector[C] mu2 = 0.5 * m2 * eta0;
  vector[C] mu3 = -((x0_sq - 1.0) / 6.0) * m3;
  vector[C] mu4 = (eta0 / 24.0) .* (3.0 * (2.0 - x0_sq) * m2_sq + (x0_sq - 3.0) * m4);

  return mu2 + mu3 + mu4;
}
