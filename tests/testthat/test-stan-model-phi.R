skip_if_not_installed("rstan")
skip_if_stan_unchanged(c(
  "functions/diff.stan",
  "functions/unrolled_dose_static_lambda.stan",
  "functions/bounds_to_range.stan",
  "transformed_data/layer_indices.stan",
  "transformed_data/single_indices.stan",
  "model/hierarchical_phi.stan",
  "model/single_phi.stan"
))

code_hierarchical_phi <- "
functions {
  #include functions/diff.stan
  #include functions/unrolled_dose_static_lambda.stan
  #include functions/bounds_to_range.stan
}
data {
  int n_obs;
  int n_weights;
  int n_cohort;
  int n_yr;
  int n_locs;
  int n_layers;
  int n_parent_locs;
  array[n_parent_locs] int parent_loc_id;
  array[2, n_parent_locs] int parent_child_bounds;
  array[n_obs] int obs_to_weights_bounds;
  array[n_weights] int weights_cohort;
  array[n_weights] int weights_location;
  array[n_weights] int weights_dose;
  array[n_weights] int weights_life_year;
  vector[n_weights] weights;
  array[2, n_layers] int layer_bounds;
  array[n_layers] int layer_sizes;

  int k_bs;
  matrix[n_cohort, k_bs] bs;
  int n_doses;
  matrix[n_yr, n_doses] dose_sched;
  real epsilon_p;

  // Deterministic parameter inputs passed via data for exact testing
  vector[k_bs] beta_bs;
  vector[n_doses] lambda_raw;
  vector[n_locs - 1] off_layer;
}
transformed data {
  #include transformed_data/layer_indices.stan
}
parameters {
  real dummy;
}
model {
  dummy ~ normal(0, 1);
}
generated quantities {
  vector[n_obs] p_obs;
  #include model/hierarchical_phi.stan
}
"

code_single_phi <- "
functions {
  #include functions/diff.stan
  #include functions/unrolled_dose_static_lambda.stan
  #include functions/bounds_to_range.stan
}
data {
  int n_obs;
  int n_weights;
  int n_cohort;
  int n_yr;
  array[n_obs] int obs_to_weights_bounds;
  array[n_weights] int weights_cohort;
  array[n_weights] int weights_dose;
  array[n_weights] int weights_life_year;
  vector[n_weights] weights;

  int k_bs;
  matrix[n_cohort, k_bs] bs;
  int n_doses;
  matrix[n_yr, n_doses] dose_sched;
  real epsilon_p;

  vector[k_bs] beta_bs;
  vector[n_doses] lambda_raw;
}
transformed data {
  #include transformed_data/single_indices.stan
}
parameters {
  real dummy;
}
model {
  dummy ~ normal(0, 1);
}
generated quantities {
  vector[n_obs] p_obs;
  #include model/single_phi.stan
}
"

model_hierarchical_phi <- compile_stan_harness(code_hierarchical_phi)
model_single_phi <- compile_stan_harness(code_single_phi)

test_that("Stan model/hierarchical_phi.stan computes observation probabilities deterministically", {
  # Minimal hierarchy: Root (1), County (2)
  n_obs <- 1L
  n_weights <- 2L
  n_cohort <- 2L
  n_yr <- 2L
  n_locs <- 2L
  n_layers <- 2L
  n_parent_locs <- 1L
  parent_loc_id <- as.array(1L)
  parent_child_bounds <- matrix(c(2L, 2L), nrow = 2, ncol = 1)
  layer_bounds <- matrix(c(1L, 1L, 2L, 2L), nrow = 2, ncol = 2)
  layer_sizes <- c(1L, 1L)

  bs <- matrix(c(1.0, 0.0, 0.0, 1.0), nrow = 2, ncol = 2)
  dose_sched <- matrix(c(1.0, 1.0), nrow = 2, ncol = 1)

  data_list <- list(
    n_obs = n_obs,
    n_weights = n_weights,
    n_cohort = n_cohort,
    n_yr = n_yr,
    n_locs = n_locs,
    n_layers = n_layers,
    n_parent_locs = n_parent_locs,
    parent_loc_id = parent_loc_id,
    parent_child_bounds = parent_child_bounds,
    obs_to_weights_bounds = as.array(1L),
    weights_cohort = c(1L, 2L),
    weights_location = c(2L, 2L),
    weights_dose = c(1L, 1L),
    weights_life_year = c(1L, 2L),
    weights = c(0.5, 0.5),
    layer_bounds = layer_bounds,
    layer_sizes = layer_sizes,
    k_bs = 2L,
    bs = bs,
    n_doses = 1L,
    dose_sched = dose_sched,
    epsilon_p = 1e-9,
    beta_bs = c(0.0, 0.0),
    lambda_raw = as.array(log(1.0)),
    off_layer = as.array(0.0)
  )

  res <- run_stan_harness(model_hierarchical_phi, data = data_list)
  p_obs <- as.numeric(res$p_obs[1, ])

  expect_length(p_obs, 1)
  expect_true(p_obs > 0 && p_obs < 1)

  # Hand-calculated analytical reference:
  # beta_bs = 0, off_layer = 0 -> logit_phi = 0 -> phi = 0.5 -> 1 - phi = 0.5
  # lambda = 1.0, dose_sched = [1, 1]
  # cdf year 1 = 1 - exp(-1) = 0.6321206
  # cdf year 2 = 1 - exp(-2) = 0.8646647
  # weighted = 0.5 * c(0.6321206, 0.8646647) * 0.5
  # sum(weighted) = 0.25 * (0.6321206 + 0.8646647) = 0.3741963
  expected_p <- 0.25 * ((1 - exp(-1)) + (1 - exp(-2)))
  expect_equal(p_obs, expected_p, tolerance = 1e-6)
})

test_that("Stan model/single_phi.stan computes single-location observation probabilities", {
  data_list <- list(
    n_obs = 1L,
    n_weights = 2L,
    n_cohort = 2L,
    n_yr = 2L,
    obs_to_weights_bounds = as.array(1L),
    weights_cohort = c(1L, 2L),
    weights_dose = c(1L, 1L),
    weights_life_year = c(1L, 2L),
    weights = c(0.5, 0.5),
    k_bs = 2L,
    bs = matrix(c(1.0, 0.0, 0.0, 1.0), nrow = 2, ncol = 2),
    n_doses = 1L,
    dose_sched = matrix(c(1.0, 1.0), nrow = 2, ncol = 1),
    epsilon_p = 1e-9,
    beta_bs = c(0.0, 0.0), # logit_phi = 0 -> phi = 0.5
    lambda_raw = as.array(log(1.0))
  )

  res <- run_stan_harness(model_single_phi, data = data_list)
  p_obs <- as.numeric(res$p_obs[1, ])

  # Expected: (1 - 0.5) * 0.5 * ( (1 - exp(-1)) + (1 - exp(-2)) )
  expected_p <- 0.25 * ((1 - exp(-1)) + (1 - exp(-2)))
  expect_equal(p_obs, expected_p, tolerance = 1e-6)
})
