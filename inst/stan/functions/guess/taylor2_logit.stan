/**
 * @file taylor2_logit.stan
 * @brief 2nd-order Taylor initial guesser for hierarchical logit subpopulation aggregation.
 */

#include functions/guess/taylor_logit.stan

/**
 * Static 2nd-order Taylor initial guess for logit link subpopulation aggregation.
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
  return guess_shift_taylor2_logit(eta0, p0, w, delta);
}

