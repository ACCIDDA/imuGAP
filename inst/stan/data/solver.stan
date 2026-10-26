/**
 * @file solver.stan
 * @brief Rootfinding solver dispatch code for nonlinear aggregation identities.
 *
 * Integer codes:
 *   1: direct    - zero-step direct evaluation of initial guess
 *   2: halley2   - 2-iteration Halley solver (standard production default)
 *   3: newton2   - 2-iteration Newton-Raphson solver
 *   4: builtin   - Stan algebra_solver Powell hybrid rootfinder
 *   5: halley10  - 10-iteration high-precision Halley solver
 */
int<lower=1, upper=5> solver_type;
