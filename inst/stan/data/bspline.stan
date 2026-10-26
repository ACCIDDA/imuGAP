/**
 * @file bspline.stan
 * @brief B-spline temporal basis specification for top-level cohort trajectories.
 */

// Number of B-spline basis functions (degrees of freedom)
int k_bs;

// Basis design matrix evaluated across cohorts (n_cohort x k_bs)
matrix[n_cohort, k_bs] bs;
