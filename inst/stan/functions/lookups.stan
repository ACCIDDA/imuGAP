array[] int compute_cdf_lookup(array[] int life_year, array[] int dose, int n_yr) {
  int n = size(life_year);
  array[n] int cdf_lookup;
  for (i in 1:n) {
    cdf_lookup[i] = life_year[i] + (dose[i] - 1) * n_yr;
  }
  return cdf_lookup;
}

array[] int compute_phi_lookup(array[] int cohort, array[] int location, int n_cohort) {
  int n = size(cohort);
  array[n] int phi_lookup;
  for (i in 1:n) {
    phi_lookup[i] = cohort[i] + (location[i] - 1) * n_cohort;
  }
  return phi_lookup;
}

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
