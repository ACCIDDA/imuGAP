/**
 * @file logit.stan
 * @brief Inverse logit link functions mapping unbounded latent scale -> (0, 1).
 */

real inv_link(real x) {
  return inv_logit(x);
}

vector inv_link(vector x) {
  return inv_logit(x);
}

matrix inv_link(matrix x) {
  return inv_logit(x);
}

// Evaluate link-scale mu offset for logit link from enclosing parent probability and offset moments
vector evaluate_mu_from_moments(vector p_enclosing, vector moments) {
  real m2 = moments[1];
  return 0.5 * m2 * (2.0 * p_enclosing - 1.0);
}

