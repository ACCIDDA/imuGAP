/**
 * @file pade_probit.stan
 * @brief Padé [1/1] initial guesser for hierarchical probit subpopulation aggregation.
 */

#include functions/guess/pade.stan

/**
 * Static Padé [1/1] initial guess for probit link subpopulation aggregation.
 *
 * @param eta0 Vector of enclosing parent link-scale parameters (length `n_cohort`).
 * @param p0 Vector of enclosing parent probabilities (length `n_cohort`).
 * @param w Vector of normalized child population weights (length `K`).
 * @param delta Vector of child spatial offsets (length `K`).
 * @return Vector of initial link-scale shifts mu of length `n_cohort`.
 */
vector guess_subpop_shift(
  vector eta0,
  vector p0,
  data vector w,
  vector delta
) {
  return guess_shift_pade_probit(eta0, p0, w, delta);
}

