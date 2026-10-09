/**
 * @file logit.stan
 * @brief Logit and inverse logit link functions mapping latent scale <-> (0, 1).
 */

/**
 * Apply logit link function to a scalar probability.
 *
 * @param p Scalar probability in (0, 1).
 * @return Value on latent link scale: log(p / (1 - p)).
 */
real link_fn(real p) {
  return logit(p);
}

/**
 * Apply logit link function elementwise to a vector of probabilities.
 *
 * @param p Vector of probabilities in (0, 1).
 * @return Vector on latent link scale.
 */
vector link_fn(vector p) {
  return logit(p);
}

/**
 * Apply inverse logit link function to a scalar coordinate.
 *
 * @param x Scalar coordinate on link scale.
 * @return Probability in (0, 1): 1 / (1 + exp(-x)).
 */
real inv_link(real x) {
  return inv_logit(x);
}

/**
 * Apply inverse logit link function elementwise to a vector.
 *
 * @param x Vector on link scale.
 * @return Vector of probabilities in (0, 1).
 */
vector inv_link(vector x) {
  return inv_logit(x);
}

/**
 * Apply inverse logit link function elementwise to a matrix.
 *
 * @param x Matrix on link scale.
 * @return Matrix of probabilities in (0, 1).
 */
matrix inv_link(matrix x) {
  return inv_logit(x);
}

/**
 * Compute first derivative of inverse logit link function: p * (1 - p).
 *
 * @param x Vector on link scale.
 * @param p Vector of probabilities in (0, 1): inv_link(x).
 * @return Vector of first derivatives.
 */
vector d_inv_link(vector x, vector p) {
  return p .* (1.0 - p);
}

/**
 * Compute second derivative of inverse logit link function: d1 * (1 - 2p).
 *
 * @param x Vector on link scale.
 * @param p Vector of probabilities in (0, 1): inv_link(x).
 * @param d1 Vector of first derivatives: d_inv_link(x, p).
 * @return Vector of second derivatives.
 */
vector d2_inv_link(vector x, vector p, vector d1) {
  return d1 .* (1.0 - 2.0 * p);
}


