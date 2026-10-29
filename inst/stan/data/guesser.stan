/**
 * @file guesser.stan
 * @brief Initial guesser dispatch code for nonlinear aggregation identities.
 *
 * Integer codes:
 *   1: zero        - naive zero shift
 *   2: taylor2     - 2nd-order Taylor expansion
 *   3: taylor4     - 4th-order Taylor expansion
 *   4: pade        - Padé [1/1] rational approximation (probit) / taylor4 fallback (logit)
 *   5: asymptotic  - variance-scaled asymptotic expansion
 *   6: conditioned - piecewise conditioned approximation
 */
int<lower=1, upper=6> guess_type;

