// Conditioned Warm-Start Guesses for Logit and Probit Links
// Uses:
//  - Logit: Taylor4 (center), MGF Log-Exceedance (tails p0 >= 0.85 or p0 <= 0.15)
//  - Probit: Padé [1/1] (center/moderate), Asymptotic Convolution Shift (outside convergence radius)

/**
 * Baseline zero initial guess (naive warm-start).
 *
 * @param eta0 Vector of baseline coordinates on link scale.
 * @param p0 Vector of baseline target probabilities.
 * @param w Vector of child weights summing to 1.
 * @param delta Vector of relative child offsets on link scale.
 * @return Vector of zeros with same length as eta0.
 */
vector shift_zero(vector eta0, vector p0, vector w, vector delta) {
  return rep_vector(0.0, num_elements(eta0));
}

// -----------------------------------------------------------------------------
// Logit Conditioned Guess
// -----------------------------------------------------------------------------

/**
 * Asymptotic Gaussian convolution shift approximation for logit link.
 *
 * @param eta0 Vector of baseline coordinates on logit scale.
 * @param p0 Vector of baseline target probabilities (unused in asymptotic scaling).
 * @param w Vector of child weights summing to 1.
 * @param delta Vector of relative child offsets on logit scale.
 * @return Vector of asymptotic logit link-scale shifts.
 */
vector shift_logit_asymptotic(vector eta0, vector p0, vector w, vector delta) {
  real m2 = dot_product(w, square(delta));
  // Logit Gaussian convolution approximation: 3 / pi^2 ~ 0.30396355
  real scale_fac = sqrt(1.0 + (3.0 / (pi() * pi())) * m2) - 1.0;
  return eta0 * scale_fac;
}

/**
 * Piecewise conditioned warm-start guess for logit link.
 * Automatically branches between Taylor4, MGF log-exceedance, and asymptotic convolution.
 *
 * @param eta0 Vector of baseline coordinates on logit scale.
 * @param p0 Vector of baseline target probabilities (must be clamped to (0, 1) prior to call).
 * @param w Vector of child weights summing to 1.
 * @param delta Vector of relative child offsets on logit scale.
 * @return Vector of conditioned logit link-scale initial guesses.
 */
vector shift_logit_conditioned(vector eta0, vector p0, vector w, vector delta) {
  int C = num_elements(eta0);
  real max_delta = max(abs(delta));
  vector[C] mu;

  vector[C] mu_t4 = shift_logit_taylor4(eta0, p0, w, delta);
  vector[C] mu_mgf = shift_logit_mgf(eta0, p0, w, delta);
  vector[C] mu_asymp = shift_logit_asymptotic(eta0, p0, w, delta);

  for (c in 1:C) {
    real p = p0[c];
    real r_conv = pi() * fmin(p, 1.0 - p);
    real ratio = max_delta / fmax(1e-8, r_conv);

    if (p >= 0.85 || p <= 0.15) {
      mu[c] = mu_mgf[c];
    } else if (ratio < 0.8) {
      mu[c] = mu_t4[c];
    } else {
      mu[c] = mu_asymp[c];
    }
  }

  return mu;
}

// -----------------------------------------------------------------------------
// Probit Conditioned Guess
// -----------------------------------------------------------------------------

/**
 * Asymptotic convolution shift approximation for probit link.
 *
 * @param eta0 Vector of baseline coordinates on probit scale (x0 = inv_Phi(p0)).
 * @param p0 Vector of baseline target probabilities (unused in asymptotic scaling).
 * @param w Vector of child weights summing to 1.
 * @param delta Vector of relative child offsets on probit scale.
 * @return Vector of asymptotic probit link-scale shifts.
 */
vector shift_probit_asymptotic(vector eta0, vector p0, vector w, vector delta) {
  real m2 = dot_product(w, square(delta));
  real scale_fac = sqrt(1.0 + m2) - 1.0;
  return eta0 * scale_fac;
}

/**
 * Piecewise conditioned warm-start guess for probit link.
 * Automatically branches between Padé [1/1] rational approximant and asymptotic convolution.
 *
 * @param eta0 Vector of baseline coordinates on probit scale (x0 = inv_Phi(p0)).
 * @param p0 Vector of baseline target probabilities (unused in probit expansion).
 * @param w Vector of child weights summing to 1.
 * @param delta Vector of relative child offsets on probit scale.
 * @return Vector of conditioned probit link-scale initial guesses.
 */
vector shift_probit_conditioned(vector eta0, vector p0, vector w, vector delta) {
  int C = num_elements(eta0);
  real max_delta = max(abs(delta));
  vector[C] mu;

  vector[C] mu_pade = shift_probit_pade11(eta0, p0, w, delta);
  vector[C] mu_asymp = shift_probit_asymptotic(eta0, p0, w, delta);

  for (c in 1:C) {
    real x0_abs = abs(eta0[c]);
    real r_conv = 1.0 / fmax(1e-4, x0_abs);
    real ratio = max_delta / fmax(1e-8, r_conv);

    if (ratio < 1.2) {
      mu[c] = mu_pade[c];
    } else {
      mu[c] = mu_asymp[c];
    }
  }

  return mu;
}
