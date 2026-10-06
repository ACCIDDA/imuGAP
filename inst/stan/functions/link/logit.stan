/**
 * @file logit.stan
 * @brief Logit and inverse logit link functions mapping latent scale <-> (0, 1).
 */

real link_fn(real p) {
  return logit(p);
}

vector link_fn(vector p) {
  return logit(p);
}

real inv_link(real x) {
  return inv_logit(x);
}

vector inv_link(vector x) {
  return inv_logit(x);
}

matrix inv_link(matrix x) {
  return inv_logit(x);
}

vector d_inv_link(vector x) {
  vector[num_elements(x)] p = inv_logit(x);
  return p .* (1.0 - p);
}

vector d2_inv_link(vector x) {
  vector[num_elements(x)] p = inv_logit(x);
  return p .* (1.0 - p) .* (1.0 - 2.0 * p);
}

// Evaluate link-scale mu offset for logit link from enclosing parent probability and offset moments
vector evaluate_mu_from_moments(vector p_enclosing, vector moments) {
  real m2 = moments[1];
  return 0.5 * m2 * (2.0 * p_enclosing - 1.0);
}

