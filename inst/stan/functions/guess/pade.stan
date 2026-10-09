/**
 * @file pade.stan
 * @brief Padé [1/1] shift guess implementation for probit link.
 */

/**
 * Approximate link-scale shift offset mu using Padé [1/1] approximation for probit link.
 *
 * @param eta0 Vector of baseline parent coordinates on link scale (length C).
 * @param p0 Vector of baseline target probabilities (length C).
 * @param w Vector of child weights summing to 1 (length K).
 * @param delta Vector of relative child offsets on link scale (length K).
 * @return Vector of estimated shift offsets mu (length C).
 */
vector guess_shift_pade_probit(vector eta0, vector p0, data vector w, vector delta) {
  int C = num_elements(eta0);
  int K = num_elements(w);
  vector[K] delta2 = square(delta);
  real m2 = dot_product(w, delta2);
  real m3 = dot_product(w, delta .* delta2);
  real m4 = dot_product(w, square(delta2));
  real m2_sq = square(m2);
  vector[C] mu;

  for (c in 1:C) {
    real x0 = eta0[c];
    real c1 = 0.5 * m2 * x0;
    real c2 = -((square(x0) - 1.0) / 6.0) * m3;
    real c3 = (x0 / 24.0) * (3.0 * (2.0 - square(x0)) * m2_sq + (square(x0) - 3.0) * m4);

    real denom = c1 - c3;
    if (abs(c1) > 1e-10 && abs(denom) > 1e-10) {
      mu[c] = (square(c1) / denom) + c2;
    } else {
      mu[c] = c1 + c2 + c3;
    }
  }
  return mu;
}
