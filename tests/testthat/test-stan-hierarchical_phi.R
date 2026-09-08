skip_if_not_installed("rstan")
# ' "model/hierarchical_phi.stan" evaluates
# ' hierarchical spatial observation probabilities `p_obs` by accumulating
# ' baseline spline effects and multi-layer spatial random walk offsets across
# ' location hierarchies.

target <- "model/hierarchical_phi.stan"

skip_if_stan_unchanged(c(
  "functions/diff.stan",
  "functions/unrolled_dose_static_lambda.stan",
  "functions/bounds_to_range.stan",
  "transformed_data/layer_indices.stan",
  target
))

model_hierarchical_phi <- sprintf(
  "
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
  #include %s
}
",
  target
) |>
  compile_stan_harness()

test_that("hierarchical_phi.stan computes observation probabilities accurately", {
  data("locations_sim", package = "imuGAP")
  locs_sim <- canonicalize_locations(locations_sim)
  ld_sim <- assemble_layer_data(locs_sim)

  obs_bounds <- as.array(1L)
  w_cohort <- c(1L, 2L)
  w_loc <- c(2L, 2L)
  w_dose <- c(1L, 1L)
  w_life_year <- c(1L, 2L)
  weights <- c(0.5, 0.5)

  bs <- matrix(c(1.0, 0.0, 0.0, 1.0), nrow = 2L, ncol = 2L)
  dose_sched <- matrix(c(1.0, 1.0), nrow = 2L, ncol = 1L)
  beta_bs <- c(0.0, 0.0)
  lambda_val <- 1.0

  data_list <- list(
    n_obs = length(obs_bounds),
    n_weights = length(w_cohort),
    n_cohort = nrow(bs),
    n_yr = nrow(dose_sched),
    n_locs = ld_sim$n_locs,
    n_layers = ld_sim$n_layers,
    n_parent_locs = ld_sim$n_parent_locs,
    parent_loc_id = ld_sim$parent_loc_id,
    parent_child_bounds = ld_sim$parent_child_bounds,
    obs_to_weights_bounds = obs_bounds,
    weights_cohort = w_cohort,
    weights_location = w_loc,
    weights_dose = w_dose,
    weights_life_year = w_life_year,
    weights = weights,
    layer_bounds = ld_sim$layer_bounds,
    layer_sizes = ld_sim$layer_sizes,
    k_bs = ncol(bs),
    bs = bs,
    n_doses = ncol(dose_sched),
    dose_sched = dose_sched,
    epsilon_p = 1e-9,
    beta_bs = beta_bs,
    lambda_raw = as.array(log(lambda_val)),
    off_layer = rep(0.0, ld_sim$n_locs - 1L)
  )

  p_obs <- run_stan_harness(model_hierarchical_phi, data = data_list, p_obs)

  expect_length(p_obs, length(obs_bounds))
  expect_true(p_obs > 0 && p_obs < 1)

  # Analytical formula check:
  phi_inv <- 1.0 - stats::plogis(0)
  cdfs <- 1.0 - exp(-lambda_val * w_life_year)
  expected_p <- sum(weights * phi_inv * cdfs)
  expect_equal(p_obs, expected_p, tolerance = 1e-6)
})
