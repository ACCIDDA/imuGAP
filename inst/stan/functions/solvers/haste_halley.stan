// Halley's Method Rootfinding Solver (Generic Link Definition)

/**
 * Generic rootfinding solver using Halley's method for link aggregation offsets.
 *
 * @param y_guess Vector of initial guess shifts (warm-start or zero).
 * @param eta0 Vector of baseline coordinates on link scale.
 * @param p0 Vector of baseline target probabilities (assumed clamped to (0, 1) prior to call).
 * @param w Vector of child weights summing to 1.
 * @param delta Vector of relative child offsets on link scale.
 * @param max_steps Maximum number of Halley iteration steps.
 * @param tol Absolute residual tolerance stopping criterion.
 * @return Vector of solved link-scale shift offsets mu.
 */
vector solve_shift_halley(
  vector y_guess,
  vector eta0,
  vector p0,
  vector w,
  vector delta,
  int max_steps,
  real tol
) {
  int C = num_elements(p0);
  int K = num_elements(w);
  vector[C] mu = y_guess;

  for (c in 1:C) {
    vector[K] base_eta = eta0[c] + delta;

    for (s in 1:max_steps) {
      vector[K] eta = base_eta + mu[c];
      vector[K] p_i = inv_link(eta);
      real f_val = dot_product(w, p_i) - p0[c];
      if (abs(f_val) < tol) break;

      vector[K] d1 = d_inv_link(eta);
      real f1 = dot_product(w, d1);
      if (f1 <= 1e-12) break;

      vector[K] d2 = d2_inv_link(eta);
      real f2 = dot_product(w, d2);
      real denom = 2.0 * square(f1) - f_val * f2;

      if (abs(denom) <= 1e-12) {
        mu[c] = mu[c] - f_val / f1;
      } else {
        mu[c] = mu[c] - (2.0 * f_val * f1) / denom;
      }
    }
  }

  return mu;
}
