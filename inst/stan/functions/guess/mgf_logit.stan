/**
 * @file mgf_logit.stan
 * @brief MGF-based shift guess for logit link.
 */

/**
 * Approximate link-scale shift offset mu using logit tail MGF approximation.
 *
 * @param eta0 Vector of baseline parent coordinates on link scale (length C).
 * @param p0 Vector of baseline target probabilities (length C).
 * @param w Vector of child weights summing to 1 (length K).
 * @param delta Vector of relative child offsets on link scale (length K).
 * @return Vector of estimated shift offsets mu (length C).
 */
vector guess_shift_mgf_logit(vector eta0, vector p0, data vector w, vector delta) {
  int C = num_elements(p0);
  real s_neg = dot_product(w, exp(-delta));
  real s_pos = dot_product(w, exp(delta));
  real mu_high = (s_neg > 0) ? log(s_neg) : 0.0;
  real mu_low = (s_pos > 0) ? -log(s_pos) : 0.0;
  vector[C] mu;
  for (c in 1:C) {
    mu[c] = (p0[c] >= 0.5) ? mu_high : mu_low;
  }
  return mu;
}

vector guess_subpop_shift(vector eta0, vector p0, data vector w, vector delta) {
  return guess_shift_mgf_logit(eta0, p0, w, delta);
}
