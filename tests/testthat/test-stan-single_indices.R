skip_if_not_installed("rstan")
# ' "transformed_data/single_indices.stan" defines
# ' precomputed lookup index mappings (`obs_map`, `phi_lookup`, `cdf_lookup`)
# ' for single-location models.

target <- "transformed_data/single_indices.stan"

skip_if_stan_unchanged(c(
  "functions/bounds_to_range.stan",
  target
))

model_single_indices <- sprintf(
  "
functions {
  #include functions/bounds_to_range.stan
}
data {
  int n_obs;
  int n_weights;
  int n_yr;
  array[n_obs] int obs_to_weights_bounds;
  array[n_weights] int weights_cohort;
  array[n_weights] int weights_dose;
  array[n_weights] int weights_life_year;
}
transformed data {
  #include %s
}
parameters {
  real dummy;
}
model {
  dummy ~ normal(0, 1);
}
generated quantities {
  array[2, n_obs] int out_obs_map = obs_map;
  array[n_weights] int out_phi_lookup = phi_lookup;
  array[n_weights] int out_cdf_lookup = cdf_lookup;
}
",
  target
) |>
  compile_stan_harness()

test_that("single_indices.stan sets single-location lookup indices", {
  obs_bounds <- as.array(1L)
  w_cohort <- c(2L, 3L)
  w_dose <- c(1L, 2L)
  w_life_year <- c(1L, 2L)
  n_yr <- 4L

  data_single_list <- list(
    n_obs = length(obs_bounds),
    n_weights = length(w_cohort),
    n_yr = n_yr,
    obs_to_weights_bounds = obs_bounds,
    weights_cohort = w_cohort,
    weights_dose = w_dose,
    weights_life_year = w_life_year
  )

  res_single <- run_stan_harness(model_single_indices, data = data_single_list)
  expect_equal(as.numeric(res_single$out_phi_lookup[1, ]), w_cohort)
  expect_equal(
    as.numeric(res_single$out_cdf_lookup[1, ]),
    w_life_year + (w_dose - 1L) * n_yr
  )
})
