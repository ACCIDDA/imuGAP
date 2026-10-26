/**
 * @file structural.stan
 * @brief Temporal cohort dimensions, dose hazard schedules, and execution flags.
 */

// Number of discrete age years modeled per cohort (at least max observed age)
int<lower=1> n_yr;

// Number of birth year cohorts
int<lower=1> n_cohort;

// Dose hazard intervals and piecewise constant widths
int<lower=1> n_intervals;
vector<lower=0>[n_intervals] dt_vec;

// Total vaccine doses in schedule
int<lower=1> n_doses;

// Binary schedule matrix: dose_sched[m, k] = 1 if dose k is active in interval m
matrix<lower=0, upper=1>[n_intervals, n_doses] dose_sched;

// Mapping from annual age (1..n_yr) to hazard interval index (1..n_intervals)
array[n_yr] int<lower=1, upper=n_intervals> age_to_interval_map;

// Execution mode: 0 = posterior estimation, 1 = prior predictive / fitted prediction
int<lower=0, upper=1> predict_mode;

// Likelihood evaluation: 0 = skip pointwise log_lik, 1 = compute for loo / WAIC
int<lower=0, upper=1> compute_log_lik;

// Number of worker threads per chain for parallel reduce_sum likelihood evaluations
int<lower=1> num_threads;
