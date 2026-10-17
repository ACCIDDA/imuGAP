/**
 * @file direct.stan
 * @brief Direct solver (returns initial guess) for baseline aggregation.
 */

/**
 * Return initial guess directly without iterative solving.
 *
 * @param mu_init Vector of initial guess shifts (length `n_cohort`).
 * @param eta0 Vector of baseline parent coordinates on link scale (length `n_cohort`).
 * @param p0 Vector of baseline target probabilities (length `n_cohort`).
 * @param w Vector of child weights summing to 1 (length `K`).
 * @param delta Vector of relative child offsets on link scale (length `K`).
 * @return Vector of shift offsets (length `n_cohort`).
 */
vector solve_subpop_shift(
  vector mu_init,
  vector eta0,
  vector p0,
  data vector w,
  vector delta
) {
  return mu_init;
}

vector solve_subpop_shift(
  vector mu_init,
  vector eta0,
  vector p0,
  data vector w,
  vector delta,
  int solver_type
) {
  return mu_init;
}

vector solve_subpop_shift(
  vector eta0,
  vector p0,
  data vector w,
  vector delta,
  int solver_type
) {
  return guess_subpop_shift(eta0, p0, w, delta);
}

vector solve_subpop_shift(
  vector eta0,
  vector p0,
  data vector w,
  vector delta
) {
  return guess_subpop_shift(eta0, p0, w, delta);
}
