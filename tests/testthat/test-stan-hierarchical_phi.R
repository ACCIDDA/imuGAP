skip_if_not_installed("rstan")
# ' "model/hierarchical_phi.stan" evaluates
# ' hierarchical spatial observation probabilities `p_obs_*` by accumulating
# ' baseline spline effects and multi-layer spatial random walk offsets across
# ' location hierarchies.

target <- "model/hierarchical_phi.stan"

skip_if_stan_unchanged(c(
  "functions/diff.stan",
  "functions/unrolled_dose_static_lambda.stan",
  "functions/bounds_to_range.stan",
  "functions/lookups.stan",
  "transformed_data/common_indices.stan",
  "transformed_data/layer_indices.stan",
  "model/common_phi.stan",
  target
))

model_hierarchical_phi <- sprintf(
  "
functions {
  #include functions/diff.stan
  #include functions/unrolled_dose_static_lambda.stan
  #include functions/bounds_to_range.stan
  #include functions/lookups.stan
}
data {
  int<lower=0> n_obs_uncensored;
  int<lower=0> n_weights_uncensored;
  array[n_obs_uncensored] int obs_to_weights_bounds_uncensored;
  array[n_weights_uncensored] int weights_cohort_uncensored;
  array[n_weights_uncensored] int weights_location_uncensored;
  array[n_weights_uncensored] int weights_dose_uncensored;
  array[n_weights_uncensored] int weights_life_year_uncensored;
  vector[n_weights_uncensored] weights_uncensored;

  int<lower=0> n_obs_right;
  int<lower=0> n_weights_right;
  array[n_obs_right] int obs_to_weights_bounds_right;
  array[n_weights_right] int weights_cohort_right;
  array[n_weights_right] int weights_location_right;
  array[n_weights_right] int weights_dose_right;
  array[n_weights_right] int weights_life_year_right;
  vector[n_weights_right] weights_right;

  int<lower=0> n_obs_left;
  int<lower=0> n_weights_left;
  array[n_obs_left] int obs_to_weights_bounds_left;
  array[n_weights_left] int weights_cohort_left;
  array[n_weights_left] int weights_location_left;
  array[n_weights_left] int weights_dose_left;
  array[n_weights_left] int weights_life_year_left;
  vector[n_weights_left] weights_left;

  int n_cohort;
  int n_yr;
  int n_locs;
  int n_layers;
  int n_parent_locs;
  array[n_parent_locs] int parent_loc_id;
  array[2, n_parent_locs] int parent_child_bounds;
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
    n_obs_uncensored = length(obs_bounds),
    n_weights_uncensored = length(w_cohort),
    obs_to_weights_bounds_uncensored = obs_bounds,
    weights_cohort_uncensored = w_cohort,
    weights_location_uncensored = w_loc,
    weights_dose_uncensored = w_dose,
    weights_life_year_uncensored = w_life_year,
    weights_uncensored = weights,
    n_obs_right = 0L,
    n_weights_right = 0L,
    obs_to_weights_bounds_right = integer(0),
    weights_cohort_right = integer(0),
    weights_location_right = integer(0),
    weights_dose_right = integer(0),
    weights_life_year_right = integer(0),
    weights_right = numeric(0),
    n_obs_left = 0L,
    n_weights_left = 0L,
    obs_to_weights_bounds_left = integer(0),
    weights_cohort_left = integer(0),
    weights_location_left = integer(0),
    weights_dose_left = integer(0),
    weights_life_year_left = integer(0),
    weights_left = numeric(0),
    n_cohort = nrow(bs),
    n_yr = nrow(dose_sched),
    n_locs = ld_sim$n_locs,
    n_layers = ld_sim$n_layers,
    n_parent_locs = ld_sim$n_parent_locs,
    parent_loc_id = ld_sim$parent_loc_id,
    parent_child_bounds = ld_sim$parent_child_bounds,
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

  p_obs <- run_stan_harness(model_hierarchical_phi, data = data_list, p_obs_uncensored)

  expect_length(p_obs, length(obs_bounds))
  expect_true(p_obs > 0 && p_obs < 1)

  # Analytical formula check:
  phi_inv <- 1.0 - stats::plogis(0)
  cdfs <- 1.0 - exp(-lambda_val * w_life_year)
  expected_p <- sum(weights * phi_inv * cdfs)
  expect_equal(p_obs, expected_p, tolerance = 1e-6)
})
