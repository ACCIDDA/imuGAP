
  vector[n_doses] lambda = exp(lambda_raw);

  // the unconditional cdfs, given FoV + whatever remains; normalization factors
  matrix[n_yr, n_doses] dXcdf, dXpdf, normdXpdf;
  vector[n_doses] rem;

  for (dose in 1:n_doses) {
    dXcdf[, dose] = 1 - exp(-cumulative_sum(dose_sched[, dose] * lambda[dose]));
    rem[dose] = 1 - dXcdf[n_yr, dose];
    dXpdf[1, dose] = dXcdf[1, dose];
    dXpdf[2:,dose] = diff(dXcdf[, dose]);
    normdXpdf[, dose] = reverse(cumulative_sum(reverse(dXpdf[, dose]))) + rem[dose];
  }

  matrix[n_yr, n_doses] conditional_dXpdf = rep_matrix(0, n_yr, n_doses), conditional_dXcdf;
  conditional_dXpdf[, 1] = dXpdf[, 1];
  conditional_dXcdf[, 1] = dXcdf[, 1];

  for (dose in 2:n_doses) {
    int prev_dose = dose - 1;
    // conditional probability => probability got dose n-1 at some time, then probability got dose n at later times
    for (ly in 1:n_yr) {
      conditional_dXpdf[ly:, dose] += conditional_dXpdf[ly, prev_dose] * dXpdf[ly:, dose] / normdXpdf[ly, dose];
    }
    conditional_dXcdf[, dose] = cumulative_sum(conditional_dXpdf[, dose]);
  }

  // TODO check unrolling of cdfs - should be dose 1 all years, then dose 2 all years, etc
  vector[n_doses * n_yr] unrolled_dose_cdf = to_vector(conditional_dXcdf);