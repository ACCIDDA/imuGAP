// Explicit Newton-Raphson Rootfinding Solver (Generic Link Definition)

/**
 * Generic rootfinding solver using Newton-Raphson iteration for link aggregation offsets.
 *
 * @param y_guess Vector of initial guess shifts (warm-start or zero).
 * @param eta0 Vector of baseline coordinates on link scale.
 * @param p0 Vector of baseline target probabilities (assumed clamped to (0, 1) prior to call).
 * @param w Vector of child weights summing to 1.
 * @param delta Vector of relative child offsets on link scale.
 * @param max_steps Maximum number of Newton iteration steps.
 * @return Vector of solved link-scale shift offsets mu.
 */
vector solve_shift_newton(
  vector y_guess,
  vector eta0,
  vector p0,
  data vector w,
  vector delta,
  int max_steps
) {
  int C = num_elements(p0);
  int K = num_elements(w);
  vector[C] mu = y_guess;

  for (c in 1:C) {
    for (s in 1:max_steps) {
      vector[K] eta = (eta0[c] + mu[c]) + delta;
      vector[K] p_i = inv_link(eta);
      real f_val = dot_product(w, p_i) - p0[c];
      if (abs(f_val) < 1e-10) break;

      vector[K] d1 = d_inv_link(eta, p_i);
      real f1 = dot_product(w, d1);
      if (f1 <= 1e-12) break;

      mu[c] = mu[c] - f_val / f1;
    }
  }

  return mu;
}
