/**
 * @file observations.stan (right-censored)
 * @brief Right-censored binomial observation streams (unmixed and population-mixed).
 */

// --- Unmixed Observations (single cohort, age, dose, and location entity) ---
int<lower=0> n_obs_unmixed_right;
array[n_obs_unmixed_right] int<lower=0> y_obs_unmixed_right;     // Observed event counts
array[n_obs_unmixed_right] int<lower=0> y_smp_unmixed_right;     // Sample size (total trials)
array[n_obs_unmixed_right] int<lower=1, upper=n_cohort> w_cohort_unmixed_right;
array[n_obs_unmixed_right] int<lower=1, upper=n_yr> w_age_unmixed_right;
array[n_obs_unmixed_right] int<lower=1, upper=n_doses> w_dose_unmixed_right;

// --- Mixed Observations (aggregated across multiple sub-strata or sub-locations) ---
int<lower=0> n_obs_mixed_right;
array[n_obs_mixed_right] int<lower=0> y_obs_mixed_right;         // Observed event counts
array[n_obs_mixed_right] int<lower=0> y_smp_mixed_right;         // Total trials
int<lower=0> n_weights_mixed_right;
array[n_obs_mixed_right] int<lower=1, upper=n_weights_mixed_right> obs_bounds_mixed_right;
array[n_weights_mixed_right] int<lower=1, upper=n_cohort> w_cohort_mixed_right;
array[n_weights_mixed_right] int<lower=1, upper=n_yr> w_age_mixed_right;
array[n_weights_mixed_right] int<lower=1, upper=n_doses> w_dose_mixed_right;
vector<lower=0, upper=1>[n_weights_mixed_right] weights_mixed_right; // Stratum mixture weights
