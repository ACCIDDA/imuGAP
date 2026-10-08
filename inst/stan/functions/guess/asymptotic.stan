/**
 * @file asymptotic.stan
 * @brief Asymptotic variance-scaling shift guess implementations for logit and probit links.
 */

/**
 * Approximate link-scale shift offset mu using asymptotic variance scaling for logit link.
 *
 * @param eta0 Vector of baseline parent coordinates on link scale (length C).
 * @param p0 Vector of baseline target probabilities (length C).
 * @param w Vector of child weights summing to 1 (length K).
 * @param delta Vector of relative child offsets on link scale (length K).
 * @return Vector of estimated shift offsets mu (length C).
 */
vector guess_shift_asymptotic_logit(vector eta0, vector p0, data vector w, vector delta) {
  real m2 = dot_product(w, square(delta));
  real scale_fac = sqrt(1.0 + (3.0 / (pi() * pi())) * m2) - 1.0;
  return eta0 * scale_fac;
}

/**
 * Approximate link-scale shift offset mu using asymptotic variance scaling for probit link.
 *
 * @param eta0 Vector of baseline parent coordinates on link scale (length C).
 * @param p0 Vector of baseline target probabilities (length C).
 * @param w Vector of child weights summing to 1 (length K).
 * @param delta Vector of relative child offsets on link scale (length K).
 * @return Vector of estimated shift offsets mu (length C).
 */
vector guess_shift_asymptotic_probit(vector eta0, vector p0, data vector w, vector delta) {
  real m2 = dot_product(w, square(delta));
  real scale_fac = sqrt(1.0 + m2) - 1.0;
  return eta0 * scale_fac;
}
