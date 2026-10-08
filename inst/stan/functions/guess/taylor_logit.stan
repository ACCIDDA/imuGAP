/**
 * @file taylor_logit.stan
 * @brief Taylor series shift guess implementations for logit link.
 */

/**
 * Approximate logit link aggregation shift via 2nd-order Taylor expansion.
 *
 * @param eta0 Vector of baseline parent coordinates on logit scale (length C).
 * @param p0 Vector of baseline target probabilities (length C).
 * @param w Vector of child weights summing to 1 (length K).
 * @param delta Vector of relative child offsets on logit scale (length K).
 * @return Vector of 2nd-order shift approximations mu (length C).
 */
vector guess_shift_taylor2_logit(vector eta0, vector p0, data vector w, vector delta) {
  real m2 = dot_product(w, square(delta));
  return 0.5 * m2 * (2.0 * p0 - 1.0);
}

/**
 * Approximate logit link aggregation shift via 4th-order Taylor expansion.
 *
 * @param eta0 Vector of baseline parent coordinates on logit scale (length C).
 * @param p0 Vector of baseline target probabilities (length C).
 * @param w Vector of child weights summing to 1 (length K).
 * @param delta Vector of relative child offsets on logit scale (length K).
 * @return Vector of 4th-order shift approximations mu (length C).
 */
vector guess_shift_taylor4_logit(vector eta0, vector p0, data vector w, vector delta) {
  int C = num_elements(p0);
  int K = num_elements(w);
  vector[K] delta2 = square(delta);
  real m2 = dot_product(w, delta2);
  real m3 = dot_product(w, delta .* delta2);
  real m4 = dot_product(w, square(delta2));
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
