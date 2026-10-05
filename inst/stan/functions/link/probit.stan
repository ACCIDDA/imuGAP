/**
 * @file probit.stan
 * @brief Inverse probit (standard normal CDF) link functions mapping unbounded latent scale -> (0, 1).
 */

real inv_link(real x) {
  return Phi(x);
}

vector inv_link(vector x) {
  return Phi(x);
}

matrix inv_link(matrix x) {
  return Phi(x);
}

// Evaluate link-scale mu offset for probit link from enclosing parent probability and offset moments
vector evaluate_mu_from_moments(vector p_enclosing, vector moments) {
  real m2 = moments[1];
  return 0.5 * m2 * inv_Phi(p_enclosing);
}
