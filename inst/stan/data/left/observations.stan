/**
 * @file observations.stan (left-censored)
 * @brief Left-censored binomial observation streams (unmixed and population-mixed).
 */

// --- Unmixed Observations (single cohort, age, dose, and location entity) ---
int<lower=0> n_obs_unmixed_left;
array[n_obs_unmixed_left] int<lower=0> y_obs_unmixed_left;     // Observed positive counts
array[n_obs_unmixed_left] int<lower=0> y_smp_unmixed_left;     // Sample size (total trials)
array[n_obs_unmixed_left] int<lower=1, upper=n_cohort> w_cohort_unmixed_left;
array[n_obs_unmixed_left] int<lower=1, upper=n_yr> w_age_unmixed_left;
array[n_obs_unmixed_left] int<lower=1, upper=n_doses> w_dose_unmixed_left;

// --- Mixed Observations (aggregated across multiple sub-strata or sub-locations) ---
int<lower=0> n_obs_mixed_left;
array[n_obs_mixed_left] int<lower=0> y_obs_mixed_left;         // Observed positive counts
array[n_obs_mixed_left] int<lower=0> y_smp_mixed_left;         // Total trials
int<lower=0> n_weights_mixed_left;
array[n_obs_mixed_left] int<lower=1, upper=n_weights_mixed_left> obs_bounds_mixed_left;
array[n_weights_mixed_left] int<lower=1, upper=n_cohort> w_cohort_mixed_left;
array[n_weights_mixed_left] int<lower=1, upper=n_yr> w_age_mixed_left;
array[n_weights_mixed_left] int<lower=1, upper=n_doses> w_dose_mixed_left;
vector<lower=0, upper=1>[n_weights_mixed_left] weights_mixed_left; // Stratum mixture weights
