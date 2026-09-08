skip_if_not_installed("rstan")
# ' "transformed_data/single_indices.stan" defines
# ' precomputed lookup index mappings (`obs_map_*`, `phi_lookup_*`, `cdf_lookup_*`)
# ' for single-location models.

target <- "transformed_data/single_indices.stan"

skip_if_stan_unchanged(c(
  "functions/bounds_to_range.stan",
  "functions/lookups.stan",
  "transformed_data/common_indices.stan",
  target
))

model_single_indices <- sprintf(
  "
functions {
  #include functions/bounds_to_range.stan
  #include functions/lookups.stan
}
data {
  int<lower=0> n_obs_uncensored;
  int<lower=0> n_weights_uncensored;
  array[n_obs_uncensored] int obs_to_weights_bounds_uncensored;
  array[n_weights_uncensored] int weights_cohort_uncensored;
  array[n_weights_uncensored] int weights_dose_uncensored;
  array[n_weights_uncensored] int weights_life_year_uncensored;

  int<lower=0> n_obs_right;
  int<lower=0> n_weights_right;
  array[n_obs_right] int obs_to_weights_bounds_right;
  array[n_weights_right] int weights_cohort_right;
  array[n_weights_right] int weights_dose_right;
  array[n_weights_right] int weights_life_year_right;

  int<lower=0> n_obs_left;
  int<lower=0> n_weights_left;
  array[n_obs_left] int obs_to_weights_bounds_left;
  array[n_weights_left] int weights_cohort_left;
  array[n_weights_left] int weights_dose_left;
  array[n_weights_left] int weights_life_year_left;

  int n_yr;
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
  array[2, n_obs_uncensored] int out_obs_map_unc = obs_map_uncensored;
  array[n_weights_uncensored] int out_phi_lookup_unc = phi_lookup_uncensored;
  array[n_weights_uncensored] int out_cdf_lookup_unc = cdf_lookup_uncensored;
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
    n_obs_uncensored = length(obs_bounds),
    n_weights_uncensored = length(w_cohort),
    obs_to_weights_bounds_uncensored = obs_bounds,
    weights_cohort_uncensored = w_cohort,
    weights_dose_uncensored = w_dose,
    weights_life_year_uncensored = w_life_year,
    n_obs_right = 0L,
    n_weights_right = 0L,
    obs_to_weights_bounds_right = integer(0),
    weights_cohort_right = integer(0),
    weights_dose_right = integer(0),
    weights_life_year_right = integer(0),
    n_obs_left = 0L,
    n_weights_left = 0L,
    obs_to_weights_bounds_left = integer(0),
    weights_cohort_left = integer(0),
    weights_dose_left = integer(0),
    weights_life_year_left = integer(0),
    n_yr = n_yr
  )

  phi_lookup <- run_stan_harness(model_single_indices, data = data_single_list, out_phi_lookup_unc)
  expect_equal(as.numeric(phi_lookup), w_cohort)

  cdf_lookup <- run_stan_harness(model_single_indices, data = data_single_list, out_cdf_lookup_unc)
  expect_equal(
    as.numeric(cdf_lookup),
    w_life_year + (w_dose - 1L) * n_yr
  )
})
