/**
 * @file zero_probit.stan
 * @brief Zero initial guess wrapper for probit link.
 */

#include functions/guess/zero.stan

vector guess_subpop_shift(vector eta0, vector p0, data vector w, vector delta) {
  return guess_shift_zero(eta0, p0, w, delta);
}
