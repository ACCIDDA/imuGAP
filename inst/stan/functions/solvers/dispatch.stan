/**
 * @file dispatch.stan
 * @brief Unified dynamic rootfinding solver dispatcher.
 */

#include functions/solvers/haste_halley.stan
#include functions/solvers/newton.stan
#include functions/solvers/builtin_rootfinding.stan

/**
 * Dispatch rootfinding solver based on integer type code.
 *
 * Types:
 *   1: direct (direct evaluation of initial guess, 0 steps)
 *   2: halley2 (2-iteration Halley solver)
 *   3: newton2 (2-iteration Newton-Raphson solver)
 *   4: builtin (Stan algebra_solver rootfinder)
 *   5: halley10 (10-iteration high-precision Halley solver)
 *
 * @param mu_init Vector of initial guess shifts (length C).
 * @param eta0 Vector of baseline parent coordinates on link scale (length C).
 * @param p0 Vector of baseline target probabilities (length C).
 * @param w Vector of child weights summing to 1 (length K).
 * @param delta Vector of relative child offsets on link scale (length K).
 * @param solver_type Integer code specifying solver (1..5).
 * @return Vector of solved link-scale shift offsets mu (length C).
 */
vector solve_subpop_shift(
  vector mu_init,
  vector eta0,
  vector p0,
  data vector w,
  vector delta,
  data int solver_type
) {
  if (solver_type == 1) {
    return mu_init;
  } else if (solver_type == 2) {
    return solve_shift_halley(mu_init, eta0, p0, w, delta, 2);
  } else if (solver_type == 3) {
    return solve_shift_newton(mu_init, eta0, p0, w, delta, 2);
  } else if (solver_type == 4) {
    return solve_shift_builtin(mu_init, eta0, p0, w, delta);
  } else if (solver_type == 5) {
    return solve_shift_halley(mu_init, eta0, p0, w, delta, 10);
  }
  return mu_init;
}

vector solve_subpop_shift(
  vector mu_init,
  vector eta0,
  vector p0,
  data vector w,
  vector delta
) {
  return solve_subpop_shift(mu_init, eta0, p0, w, delta, 1);
}
