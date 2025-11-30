  // TODO: calculate these in stan?
  // https://spinkney.github.io/helpful_stan_functions/group__splines.html
  // state-level basis spline
  int k_bs; // number of bspline basis functions
  matrix[n_cohort, k_bs] bs; // basis functions
