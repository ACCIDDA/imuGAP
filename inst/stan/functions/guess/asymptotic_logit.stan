/**
 * @file asymptotic_logit.stan
 * @brief Asymptotic variance-scaling shift guess for logit link.
 */

#include functions/guess/asymptotic.stan

/**
 * Approximate link-scale shift offset mu using asymptotic variance scaling for logit link.
 *
 * @param eta0 Vector of baseline parent coordinates on link scale (length C).
 * @param p0 Vector of baseline target probabilities (length C).
 * @param w Vector of child weights summing to 1 (length K).
 * @param delta Vector of relative child offsets on link scale (length K).
 * @return Vector of estimated shift offsets mu (length C).
 */
vector guess_subpop_shift(vector eta0, vector p0, data vector w, vector delta) {
  return guess_shift_asymptotic_logit(eta0, p0, w, delta);
}
