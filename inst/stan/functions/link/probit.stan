/**
 * @file probit.stan
 * @brief Probit and inverse probit link functions mapping latent scale <-> (0, 1).
 */

real link_fn(real p) {
  return inv_Phi(p);
}

vector link_fn(vector p) {
  return inv_Phi(p);
}

real inv_link(real x) {
  return Phi(x);
}

vector inv_link(vector x) {
  return Phi(x);
}

matrix inv_link(matrix x) {
  return Phi(x);
}

vector d_inv_link(vector x) {
  return exp(-0.5 * square(x)) / sqrt(2.0 * pi());
}

vector d2_inv_link(vector x) {
  vector[num_elements(x)] phi = exp(-0.5 * square(x)) / sqrt(2.0 * pi());
  return -x .* phi;
}

// Evaluate link-scale mu offset for probit link from enclosing parent probability and offset moments
vector evaluate_mu_from_moments(vector p_enclosing, vector moments) {
  real m2 = moments[1];
  return 0.5 * m2 * inv_Phi(p_enclosing);
}
