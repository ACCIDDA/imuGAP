/**
 * @file zero.stan
 * @brief Zero initial guess for link aggregation shift (link-invariant).
 */

/**
 * Return zero initial guess vector for subpopulation shifts.
 *
 * @param eta0 Vector of baseline parent coordinates on link scale (length C).
 * @param p0 Vector of baseline target probabilities (length C).
 * @param w Vector of child weights summing to 1 (length K).
 * @param delta Vector of relative child offsets on link scale (length K).
 * @return Zero vector of length C.
 */
vector guess_shift_zero(vector eta0, vector p0, data vector w, vector delta) {
  return rep_vector(0.0, num_elements(eta0));
}
