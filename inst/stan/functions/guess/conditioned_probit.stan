/**
 * @file conditioned_probit.stan
 * @brief Conditioned piecewise shift guess for probit link.
 */

#include functions/guess/pade.stan
#include functions/guess/asymptotic.stan

/**
 * Approximate link-scale shift offset mu using conditioned piecewise approximation for probit link.
 *
 * @param eta0 Vector of baseline parent coordinates on link scale (length C).
 * @param p0 Vector of baseline target probabilities (length C).
 * @param w Vector of child weights summing to 1 (length K).
 * @param delta Vector of relative child offsets on link scale (length K).
 * @return Vector of estimated shift offsets mu (length C).
 */
vector guess_shift_conditioned_probit(vector eta0, vector p0, data vector w, vector delta) {
  int C = num_elements(eta0);
  vector[C] mu_pade = guess_shift_pade_probit(eta0, p0, w, delta);
  vector[C] mu_asymp = guess_shift_asymptotic_probit(eta0, p0, w, delta);

  real max_delta = max(abs(delta));
  vector[C] mu;

  for (c in 1:C) {
    real x0 = eta0[c];
    real x0_abs = abs(x0);
    real r_conv = 1.0 / fmax(1e-4, x0_abs);
    real ratio = max_delta / fmax(1e-8, r_conv);

    mu[c] = (ratio < 1.2) ? mu_pade[c] : mu_asymp[c];
  }
  return mu;
}

