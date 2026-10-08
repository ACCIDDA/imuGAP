/**
 * @file probit.stan
 * @brief Probit and inverse probit link functions mapping latent scale <-> (0, 1).
 */

/**
 * Apply probit link function to a scalar probability.
 *
 * @param p Scalar probability in (0, 1).
 * @return Value on latent link scale: inv_Phi(p).
 */
real link_fn(real p) {
  return inv_Phi(p);
}

/**
 * Apply probit link function elementwise to a vector of probabilities.
 *
 * @param p Vector of probabilities in (0, 1).
 * @return Vector on latent link scale.
 */
vector link_fn(vector p) {
  return inv_Phi(p);
}

/**
 * Apply inverse probit link function to a scalar coordinate.
 *
 * @param x Scalar coordinate on link scale.
 * @return Probability in (0, 1): Phi(x).
 */
real inv_link(real x) {
  return Phi(x);
}

/**
 * Apply inverse probit link function elementwise to a vector.
 *
 * @param x Vector on link scale.
 * @return Vector of probabilities in (0, 1).
 */
vector inv_link(vector x) {
  return Phi(x);
}

/**
 * Apply inverse probit link function elementwise to a matrix.
 *
 * @param x Matrix on link scale.
 * @return Matrix of probabilities in (0, 1).
 */
matrix inv_link(matrix x) {
  return Phi(x);
}

/**
 * Compute first derivative of inverse probit link function: phi(x) = exp(-0.5 * x^2) / sqrt(2 * pi).
 *
 * @param x Vector on link scale.
 * @param p Vector of probabilities in (0, 1): inv_link(x).
 * @return Vector of standard normal PDF evaluations.
 */
vector d_inv_link(vector x, vector p) {
  return exp(-0.5 * square(x)) / sqrt(2.0 * pi());
}

/**
 * Compute second derivative of inverse probit link function: -x * d1.
 *
 * @param x Vector on link scale.
 * @param p Vector of probabilities in (0, 1): inv_link(x).
 * @param d1 Vector of first derivatives: d_inv_link(x, p).
 * @return Vector of second derivatives.
 */
vector d2_inv_link(vector x, vector p, vector d1) {
  return -x .* d1;
}


