/**
 * @file direct.stan
 * @brief Direct solver (returns initial guess) for baseline aggregation.
 */

/**
 * Return initial guess directly without iterative solving.
 *
 * @param eta0 Vector of enclosing parent link-scale parameters (length `n_cohort`).
 * @param p0 Vector of enclosing parent probabilities (length `n_cohort`).
 * @param w Vector of normalized child population weights (length `K`).
 * @param delta Vector of child spatial offsets (length `K`).
 * @return Vector of shift offsets.
 */
vector solve_subpop_shift(
  vector eta0,
  vector p0,
  vector w,
  vector delta
) {
  return guess_subpop_shift(eta0, p0, w, delta);
}

