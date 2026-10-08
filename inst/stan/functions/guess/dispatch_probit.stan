/**
 * @file dispatch_probit.stan
 * @brief Unified dynamic initial guesser dispatcher for probit link.
 */

#include functions/guess/zero.stan
#include functions/guess/taylor_probit.stan
#include functions/guess/conditioned_probit.stan

/**
 * Dispatch initial guesser for probit link based on integer type code.
 *
 * Types:
 *   1: zero (naive 0.0)
 *   2: taylor2 (2nd-order Taylor)
 *   3: taylor4 (4th-order Taylor)
 *   4: pade ([1/1] Padé rational polynomial)
 *   5: asymptotic (variance-scaled)
 *   6: conditioned (piecewise optimal)
 *
 * @param eta0 Vector of baseline parent coordinates on probit scale (length C).
 * @param p0 Vector of baseline target probabilities (length C).
 * @param w Vector of child weights summing to 1 (length K).
 * @param delta Vector of relative child offsets on probit scale (length K).
 * @param guess_type Integer code specifying guesser (1..6).
 * @return Vector of estimated shift offsets mu (length C).
 */
vector guess_subpop_shift(
  vector eta0,
  vector p0,
  data vector w,
  vector delta,
  data int guess_type
) {
  if (guess_type == 1) {
    return guess_shift_zero(eta0, p0, w, delta);
  } else if (guess_type == 2) {
    return guess_shift_taylor2_probit(eta0, p0, w, delta);
  } else if (guess_type == 3) {
    return guess_shift_taylor4_probit(eta0, p0, w, delta);
  } else if (guess_type == 4) {
    return guess_shift_pade_probit(eta0, p0, w, delta);
  } else if (guess_type == 5) {
    return guess_shift_asymptotic_probit(eta0, p0, w, delta);
  } else if (guess_type == 6) {
    return guess_shift_conditioned_probit(eta0, p0, w, delta);
  }
  return guess_shift_taylor4_probit(eta0, p0, w, delta);
}

vector guess_subpop_shift(
  vector eta0,
  vector p0,
  data vector w,
  vector delta
) {
  return guess_subpop_shift(eta0, p0, w, delta, 3);
}
