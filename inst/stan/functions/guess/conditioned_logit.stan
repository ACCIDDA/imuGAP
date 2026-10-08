/**
 * @file conditioned_logit.stan
 * @brief Conditioned piecewise shift guess for logit link.
 */

#include functions/guess/taylor_logit.stan
#include functions/guess/asymptotic.stan

/**
 * Approximate link-scale shift offset mu using conditioned piecewise approximation for logit link.
 *
 * @param eta0 Vector of baseline parent coordinates on link scale (length C).
 * @param p0 Vector of baseline target probabilities (length C).
 * @param w Vector of child weights summing to 1 (length K).
 * @param delta Vector of relative child offsets on link scale (length K).
 * @return Vector of estimated shift offsets mu (length C).
 */
vector guess_shift_conditioned_logit(vector eta0, vector p0, data vector w, vector delta) {
  int C = num_elements(p0);
  vector[C] mu_t4 = guess_shift_taylor4_logit(eta0, p0, w, delta);
  vector[C] mu_asymp = guess_shift_asymptotic_logit(eta0, p0, w, delta);

  real s_neg = dot_product(w, exp(-delta));
  real s_pos = dot_product(w, exp(delta));
  real mu_tail_high = (s_neg > 0) ? log(s_neg) : 0.0;
  real mu_tail_low = (s_pos > 0) ? -log(s_pos) : 0.0;

  real max_delta = max(abs(delta));
  vector[C] mu;

  for (c in 1:C) {
    real p = p0[c];
    real r_conv = pi() * fmin(p, 1.0 - p);
    real ratio = max_delta / fmax(1e-8, r_conv);

    if (p >= 0.85) {
      mu[c] = mu_tail_high;
    } else if (p <= 0.15) {
      mu[c] = mu_tail_low;
    } else if (ratio < 0.8) {
      mu[c] = mu_t4[c];
    } else {
      mu[c] = mu_asymp[c];
    }
  }
  return mu;
}

