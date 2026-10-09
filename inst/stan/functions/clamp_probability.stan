/**
 * @file clamp_probability.stan
 * @brief Probability boundary clamping to prevent underflow/overflow in link transformations.
 */

/**
 * Clamp a scalar probability to (1e-12, 1 - 1e-12).
 *
 * @param p Scalar probability.
 * @return Clamped scalar probability in [1e-12, 1 - 1e-12].
 */
real clamp_probability(real p) {
  return fmax(1e-12, fmin(1.0 - 1e-12, p));
}

/**
 * Clamp a vector of probabilities elementwise to (1e-12, 1 - 1e-12).
 *
 * @param p Vector of probabilities.
 * @return Clamped vector of probabilities in [1e-12, 1 - 1e-12].
 */
vector clamp_probability(vector p) {
  return fmax(1e-12, fmin(1.0 - 1e-12, p));
}
