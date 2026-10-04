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
