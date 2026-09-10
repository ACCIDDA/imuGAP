// The unrolled dose CDF vector is arranged in column-major order:
// all life years for dose 1 (1:n_yr), then all life years for dose 2, up to dose n_doses.
// This function maps (life_year, dose) pairs to their 1D index: life_year + (dose - 1) * n_yr.
array[] int compute_cdf_lookup(array[] int life_year, array[] int dose, int n_yr, int n_doses) {
  int n_ly = size(life_year);
  int n_d = size(dose);
  if (n_ly != n_d) {
    reject("Array size mismatch: size(life_year) = ", n_ly, " != size(dose) = ", n_d);
  }
  if (n_yr < 1) {
    reject("n_yr must be >= 1, but found n_yr = ", n_yr);
  }
  if (n_doses < 1) {
    reject("n_doses must be >= 1, but found n_doses = ", n_doses);
  }
  array[n_ly] int cdf_lookup;
  for (i in 1:n_ly) {
    if (life_year[i] < 1 || life_year[i] > n_yr) {
      reject("life_year[", i, "] = ", life_year[i], " is out of bounds [1, ", n_yr, "]");
    }
    if (dose[i] < 1 || dose[i] > n_doses) {
      reject("dose[", i, "] = ", dose[i], " is out of bounds [1, ", n_doses, "]");
    }
    cdf_lookup[i] = life_year[i] + (dose[i] - 1) * n_yr;
  }
  return cdf_lookup;
}

// The unrolled phi vector is arranged in column-major order:
// all cohorts for location 1 (1:n_cohort), then all cohorts for location 2, up to location n_locs.
// This function maps (cohort, location) pairs to their 1D index:
// cohort + (location - 1) * n_cohort.
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

// NOTE: phi represents the non-uptake proportion (unlikely to vaccinate).
// Therefore, (1 - phi) represents the vaccinating population (lifetime uptake propensity).
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
      p[i] = sum(weighted[obs_map[1, i]:obs_map[2, i]]);
    }
  }
  return p;
}
