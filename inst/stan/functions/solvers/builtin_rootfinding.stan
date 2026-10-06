// Stan Built-in Rootfinding Solver (algebra_solver, Generic Link Definition)

/**
 * Residual system function for Stan's built-in algebra_solver.
 *
 * @param theta 1D state vector containing candidate link-scale shift mu.
 * @param params Parameter vector containing [eta0, p0, delta_1, ..., delta_K] (p0 assumed clamped).
 * @param x_r Real data array containing sub-region weights w_1, ..., w_K.
 * @param x_i Integer data array containing sub-region count [K].
 * @return 1D vector residual dot_product(w, inv_link(eta0 + theta + delta)) - p0.
 */
vector offset_residual(
  vector theta,
  vector params,
  data array[] real x_r,
  data array[] int x_i
) {
  real eta0 = params[1];
  real p0 = params[2];
  int K = x_i[1];
  vector[K] w = to_vector(x_r[1:K]);
  vector[K] delta = params[3:(K + 2)];
  vector[1] res;

  res[1] = dot_product(w, inv_link(eta0 + theta[1] + delta)) - p0;
  return res;
}

/**
 * Generic rootfinding solver wrapping Stan's built-in algebra_solver.
 *
 * @param y_guess Vector of initial guess shifts (warm-start or zero).
 * @param eta0 Vector of baseline coordinates on link scale.
 * @param p0 Vector of baseline target probabilities (assumed clamped to (0, 1) prior to call).
 * @param w Vector of child weights summing to 1 (data qualifier).
 * @param delta Vector of relative child offsets on link scale.
 * @return Vector of solved link-scale shift offsets mu.
 */
vector solve_shift_builtin(
  vector y_guess,
  vector eta0,
  vector p0,
  data vector w,
  vector delta
) {
  int C = num_elements(p0);
  int K = num_elements(w);
  array[K] real w_r = to_array_1d(w);
  array[1] int K_i = { K };

  vector[C] theta_sol;
  vector[1] y_g;
  vector[K + 2] params;
  params[3:(K + 2)] = delta;

  for (c in 1:C) {
    y_g[1] = y_guess[c];
    params[1] = eta0[c];
    params[2] = p0[c];

    vector[1] res = algebra_solver(
      offset_residual, y_g, params, w_r, K_i,
      1e-10, 1e-8, 1000
    );
    theta_sol[c] = res[1];
  }

  return theta_sol;
}
