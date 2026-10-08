/**
 * @file pade_probit.stan
 * @brief Padé [1/1] shift guess for probit link.
 */

#include functions/guess/pade.stan

/**
 * Approximate link-scale shift offset mu using Padé [1/1] approximation for probit link.
 *
 * @param eta0 Vector of baseline parent coordinates on link scale (length C).
 * @param p0 Vector of baseline target probabilities (length C).
 * @param w Vector of child weights summing to 1 (length K).
 * @param delta Vector of relative child offsets on link scale (length K).
 * @return Vector of estimated shift offsets mu (length C).
 */
vector guess_subpop_shift(vector eta0, vector p0, data vector w, vector delta) {
  return guess_shift_pade_probit(eta0, p0, w, delta);
}
