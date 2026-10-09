/**
 * @file builtin.stan
 * @brief Stan algebra_solver rootfinder solver module.
 */

#include functions/solvers/builtin_rootfinding.stan

/**
 * Solve subpopulation shift offsets using Stan's built-in algebra_solver.
 *
 * @param mu_init Vector of initial guess shifts (length C).
 * @param eta0 Vector of baseline parent coordinates on link scale (length C).
 * @param p0 Vector of baseline target probabilities (length C).
 * @param w Vector of child weights summing to 1 (length K).
 * @param delta Vector of relative child offsets on link scale (length K).
 * @return Vector of solved link-scale shift offsets mu (length C).
 */
vector solve_subpop_shift(vector mu_init, vector eta0, vector p0, data vector w, vector delta) {
  return solve_shift_builtin(mu_init, eta0, p0, w, delta);
}

