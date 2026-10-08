/**
 * @file lookups.stan
 * @brief Indexing and probability lookup functions for unrolled hierarchical observations.
 */

/**
 * Compute 1D column-major lookup index for (life_year, dose) pairs.
 *
 * Maps life year and dose index to the corresponding entry in the flattened unrolled
 * dose CDF vector: age_to_interval_map[life_year] + (dose - 1) * n_intervals.
 *
 * @param life_year 1D array of 1-based life year/age indices.
 * @param dose 1D array of 1-based dose numbers.
 * @param n_intervals Total number of discrete time/age intervals.
 * @param age_to_interval_map Mapping from life year index to interval index.
 * @return 1D array of 1-based column-major indices into unrolled dose CDF vector.
 */
array[] int compute_cdf_lookup(
  array[] int life_year,
  array[] int dose,
  int n_intervals,
  array[] int age_to_interval_map
) {
  int n_ly = size(life_year);
  int n_d = size(dose);
  int n_ages = size(age_to_interval_map);
  if (n_ly != n_d) {
    reject("Array size mismatch: size(life_year) = ", n_ly, " != size(dose) = ", n_d);
  }
  if (n_intervals < 1) {
    reject("n_intervals must be >= 1, but found n_intervals = ", n_intervals);
  }
  array[n_ly] int cdf_lookup;
  for (i in 1:n_ly) {
    if (life_year[i] < 1 || life_year[i] > n_ages) {
      reject("life_year[", i, "] = ", life_year[i], " is out of bounds [1, ", n_ages, "]");
    }
    if (dose[i] < 1) {
      reject("dose[", i, "] = ", dose[i], " is out of bounds (< 1)");
    }
    int interval_idx = age_to_interval_map[life_year[i]];
    if (interval_idx < 1 || interval_idx > n_intervals) {
      reject("Mapped interval index ", interval_idx, " out of bounds [1, ", n_intervals, "]");
    }
    cdf_lookup[i] = interval_idx + (dose[i] - 1) * n_intervals;
  }
  return cdf_lookup;
}

/**
 * Compute 1D column-major lookup index for (cohort, location) pairs.
 *
 * Maps cohort and location index to the corresponding entry in the flattened unrolled
 * spatial propensity vector: cohort + (location - 1) * n_cohort.
 *
 * @param cohort 1D array of 1-based cohort indices.
 * @param location 1D array of 1-based location indices.
 * @param n_cohort Total number of cohorts.
 * @param n_locs Total number of hierarchy locations.
 * @return 1D array of 1-based column-major indices into unrolled phi vector.
 */
array[] int compute_phi_lookup(array[] int cohort, array[] int location, int n_cohort, int n_locs) {
  int n_c = size(cohort);
  int n_l = size(location);
  if (n_c != n_l) {
    reject("Array size mismatch: size(cohort) = ", n_c, " != size(location) = ", n_l);
  }
  if (n_cohort < 1) {
    reject("n_cohort must be >= 1, but found n_cohort = ", n_cohort);
  }
  if (n_locs < 1) {
    reject("n_locs must be >= 1, but found n_locs = ", n_locs);
  }
  array[n_c] int phi_lookup;
  for (i in 1:n_c) {
    if (cohort[i] < 1 || cohort[i] > n_cohort) {
      reject("cohort[", i, "] = ", cohort[i], " is out of bounds [1, ", n_cohort, "]");
    }
    if (location[i] < 1 || location[i] > n_locs) {
      reject("location[", i, "] = ", location[i], " is out of bounds [1, ", n_locs, "]");
    }
    phi_lookup[i] = cohort[i] + (location[i] - 1) * n_cohort;
  }
  return phi_lookup;
}

/**
 * Compute observation-level expected vaccination probabilities.
 *
 * Multiplies cohort uptake (1 - phi), dose-timing CDF, and location mixture weights,
 * summing over contributing components per observation.
 *
 * @param n_obs Number of observations.
 * @param n_weights Total number of mixture weights across all observations.
 * @param phi Vector of location-cohort non-uptake probabilities.
 * @param phi_lookup 1D array mapping mixture rows to entries in `phi`.
 * @param unrolled_dose_probs Vector of cumulative dose timing probabilities.
 * @param cdf_lookup 1D array mapping mixture rows to entries in `unrolled_dose_probs`.
 * @param weights Data vector of normalized mixture weights.
 * @param obs_map 2D array [2, n_obs] containing [start, end] indices into weights.
 * @return Vector of length `n_obs` containing expected probabilities in [0, 1].
 */
vector compute_p_obs(
  int n_obs,
  int n_weights,
  vector phi,
  array[] int phi_lookup,
  vector unrolled_dose_probs,
  array[] int cdf_lookup,
  data vector weights,
  array[,] int obs_map
) {
  vector[n_obs] p;
  if (n_obs > 0) {
    vector[n_weights] weighted = (1.0 - phi[phi_lookup]) .* unrolled_dose_probs[cdf_lookup] .* weights;
    for (i in 1:n_obs) {
      int st = obs_map[1, i];
      int en = obs_map[2, i];
      p[i] = sum(segment(weighted, st, en - st + 1));
    }
  }
  return p;
}
