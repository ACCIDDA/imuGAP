/**
 * @file zero.stan
 * @brief Zero-shift initial guesser for hierarchical subpopulation aggregation.
 */

/**
 * Compute zero shift vector as initial guess for subpopulation aggregation.
 *
 * @param eta0 Vector of enclosing parent link-scale parameters (length `n_cohort`).
 * @param p0 Vector of enclosing parent probabilities (length `n_cohort`).
 * @param w Vector of normalized child population weights (length `K`).
 * @param delta Vector of child spatial offsets (length `K`).
 * @return Vector of zeros of length `n_cohort`.
 */
vector guess_subpop_shift(
  vector eta0,
  vector p0,
  vector w,
  vector delta
) {
  return rep_vector(0.0, num_elements(eta0));
}
