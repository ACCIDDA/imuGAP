/**
 * @file halley10.stan
 * @brief High-precision 10-step Halley rootfinder solver module.
 */

#include functions/solvers/halley.stan

/**
 * Solve subpopulation shift offsets using up to 10 Halley iteration steps.
 *
 * @param mu_init Vector of initial guess shifts (length `n_cohort`).
 * @param eta0 Vector of baseline parent coordinates on link scale (length `n_cohort`).
 * @param p0 Vector of baseline target probabilities (length `n_cohort`).
 * @param w Vector of child weights summing to 1 (length `K`).
 * @param delta Vector of relative child offsets on link scale (length `K`).
 * @return Vector of solved link-scale shift offsets mu (length `n_cohort`).
 */
vector solve_subpop_shift(
  vector mu_init,
  vector eta0,
  vector p0,
  data vector w,
  vector delta
) {
  return solve_shift_halley(mu_init, eta0, p0, w, delta, 10);
}

vector solve_subpop_shift(
  vector eta0,
  vector p0,
  data vector w,
  vector delta
) {
  vector[num_elements(eta0)] mu_init = guess_subpop_shift(eta0, p0, w, delta);
  return solve_shift_halley(mu_init, eta0, p0, w, delta, 10);
}
