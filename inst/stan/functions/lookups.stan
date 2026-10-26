/**
 * @file lookups.stan
 * @brief Flattened tensor 1D index mapping and observation probability mixing.
 */

/**
 * Compute 1D lookup index into column-major unrolled dose hazard vector.
 *
 * Arranged as: dose 1 (1:n_intervals), dose 2 (1:n_intervals), ..., dose D.
 * Index formula: age_to_interval_map[life_year[i]] + (dose[i] - 1) * n_intervals.
 *
 * @param life_year Array of observation ages in life years.
 * @param dose Array of observation dose numbers.
 * @param n_intervals Total hazard intervals.
 * @param age_to_interval_map Lookup from annual age to interval index.
 * @return Array of 1-based indices into unrolled dose CDF vector.
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
 * Compute 1D lookup index into column-major unrolled phi probability vector.
 *
 * Arranged as: location 1 (1:n_cohort), location 2 (1:n_cohort), ..., location L.
 * Index formula: cohort[i] + (location[i] - 1) * n_cohort.
 *
 * @param cohort Array of observation cohort indices.
 * @param location Array of observation location IDs.
 * @param n_cohort Total number of cohorts.
 * @param n_locs Total number of locations.
 * @return Array of 1-based indices into unrolled phi vector.
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
 * Compute expected observation probability by combining non-uptake and dose CDF.
 *
 * Note: phi represents the non-uptake proportion (unlikely to vaccinate).
 * Therefore, (1 - phi) represents the vaccinating population (lifetime uptake propensity).
 *
 * @param n_obs Number of observations to evaluate.
 * @param n_weights Total number of mixture weight entries.
 * @param phi Unrolled vector of non-uptake probabilities.
 * @param phi_lookup Index array into phi for each weight entry.
 * @param unrolled_dose_probs Cumulative vaccination probability given eventual uptake.
 * @param cdf_lookup Index array into unrolled_dose_probs for each weight entry.
 * @param weights Normalized sub-stratum mixture weights.
 * @param obs_map [2, n_obs] index bounds into flattened mixture weight array.
 * @return Vector of expected observation probabilities of length n_obs.
 */
vector compute_p_obs(
  int n_obs,
  int n_weights,
  vector phi,
  array[] int phi_lookup,
  vector unrolled_dose_probs,
  array[] int cdf_lookup,
  vector weights,
  array[,] int obs_map
) {
  vector[n_obs] p;
  if (n_obs > 0) {
    vector[n_weights] weighted = (1 - phi[phi_lookup]) .* unrolled_dose_probs[cdf_lookup] .* weights;
    for (i in 1:n_obs) {
      int st = obs_map[1, i];
      int en = obs_map[2, i];
      p[i] = sum(segment(weighted, st, en - st + 1));
    }
  }
  return p;
}
