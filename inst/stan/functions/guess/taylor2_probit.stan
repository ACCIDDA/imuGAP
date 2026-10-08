/**
 * @file taylor2_probit.stan
 * @brief 2nd-order Taylor series shift guess for probit link.
 */

#include functions/guess/taylor_probit.stan

/**
 * Approximate probit link aggregation shift via 2nd-order Taylor expansion.
 *
 * @param eta0 Vector of baseline parent coordinates on probit scale (length C).
 * @param p0 Vector of baseline target probabilities (length C).
 * @param w Vector of child weights summing to 1 (length K).
 * @param delta Vector of relative child offsets on probit scale (length K).
 * @return Vector of 2nd-order shift approximations mu (length C).
 */
vector guess_subpop_shift(vector eta0, vector p0, data vector w, vector delta) {
  return guess_shift_taylor2_probit(eta0, p0, w, delta);
}
