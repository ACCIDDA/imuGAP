test_that("Stan transformed data epsilon.stan defines precision constant", {
  skip_if_not_installed("rstan")
  skip_if_stan_unchanged("transformed_data/epsilon.stan")

  code <- "
  transformed data {
    #include transformed_data/epsilon.stan
  }
  parameters {
    real dummy;
  }
  model {
    dummy ~ normal(0, 1);
  }
  generated quantities {
    real out_eps = epsilon_p;
  }
  "

  res <- run_stan_harness(code, data = list())
  expect_equal(res$out_eps[1], 1e-10)
})

test_that("Stan transformed data layer_indices.stan constructs mappings accurately", {
  skip_if_not_installed("rstan")
  skip_if_stan_unchanged(c(
    "functions/bounds_to_range.stan",
    "transformed_data/layer_indices.stan"
  ))

  code <- "
  functions {
    #include functions/bounds_to_range.stan
  }
  data {
    int n_obs;
    int n_weights;
    int n_cohort;
    int n_yr;
    int n_locs;
    int n_layers;
    array[n_obs] int obs_to_weights_bounds;
    array[n_weights] int weights_cohort;
    array[n_weights] int weights_location;
    array[n_weights] int weights_dose;
    array[n_weights] int weights_life_year;
    array[2, n_layers] int layer_bounds;
    array[n_layers] int layer_sizes;
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
    array[2, n_obs] int out_obs_map = obs_map;
    vector[n_cohort] out_shift = cohort_shift_counter;
    array[n_weights] int out_phi_lookup = phi_lookup;
    array[n_weights] int out_cdf_lookup = cdf_lookup;
    array[n_locs - 1] int out_loc_layer_idx = loc_layer_idx;
  }
  "

  # Hierarchy: Root (1), County 1 (2), County 2 (3), School 1 (4), School 2 (5)
  # Layers: Root (size 1, bounds 1..1), County (size 2, bounds 2..3), School (size 2, bounds 4..5)
  layer_bounds <- matrix(c(1, 1, 2, 3, 4, 5), nrow = 2, ncol = 3)
  layer_sizes <- c(1L, 2L, 2L)

  data_list <- list(
    n_obs = 2L,
    n_weights = 4L,
    n_cohort = 3L,
    n_yr = 5L,
    n_locs = 5L,
    n_layers = 3L,
    obs_to_weights_bounds = c(1L, 3L),
    weights_cohort = c(1L, 2L, 1L, 3L),
    weights_location = c(2L, 2L, 4L, 5L),
    weights_dose = c(1L, 1L, 2L, 2L),
    weights_life_year = c(1L, 2L, 1L, 3L),
    layer_bounds = layer_bounds,
    layer_sizes = layer_sizes
  )

  res <- run_stan_harness(code, data = data_list)

  # Check obs_map: [1, 2], [3, 4]
  obs_map <- res$out_obs_map[1, , ]
  expect_equal(obs_map[1, ], c(1, 3))
  expect_equal(obs_map[2, ], c(2, 4))

  # Check cohort shift
  expect_equal(as.numeric(res$out_shift[1, ]), c(1, 2, 3))

  # Check loc_layer_idx for 4 non-root locations:
  # county 1 & 2 -> layer 1; school 1 & 2 -> layer 2
  expect_equal(as.numeric(res$out_loc_layer_idx[1, ]), c(1, 1, 2, 2))

  # Check phi_lookup: cohort + (loc - 1) * n_cohort
  # 1 + (2-1)*3 = 4, 2 + (2-1)*3 = 5, 1 + (4-1)*3 = 10, 3 + (5-1)*3 = 15
  expect_equal(as.numeric(res$out_phi_lookup[1, ]), c(4, 5, 10, 15))

  # Check cdf_lookup: life_year + (dose - 1) * n_yr
  # 1 + 0 = 1, 2 + 0 = 2, 1 + 5 = 6, 3 + 5 = 8
  expect_equal(as.numeric(res$out_cdf_lookup[1, ]), c(1, 2, 6, 8))
})

test_that("Stan transformed data single_indices.stan sets single-location lookups", {
  skip_if_not_installed("rstan")
  skip_if_stan_unchanged(c(
    "functions/bounds_to_range.stan",
    "transformed_data/single_indices.stan"
  ))

  code <- "
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
    #include transformed_data/single_indices.stan
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
  "

  data_list <- list(
    n_obs = 1L,
    n_weights = 2L,
    n_yr = 4L,
    obs_to_weights_bounds = as.array(1L),
    weights_cohort = c(2L, 3L),
    weights_dose = c(1L, 2L),
    weights_life_year = c(1L, 2L)
  )

  res <- run_stan_harness(code, data = data_list)
  expect_equal(as.numeric(res$out_phi_lookup[1, ]), c(2, 3))
  expect_equal(as.numeric(res$out_cdf_lookup[1, ]), c(1, 6))
})

test_that("Stan transformed data censoring.stan shifts count observations", {
  skip_if_not_installed("rstan")
  skip_if_stan_unchanged("transformed_data/censoring.stan")

  code <- "
  data {
    int n_obs;
    array[n_obs] int y_obs;
  }
  transformed data {
    #include transformed_data/censoring.stan
  }
  parameters {
    real dummy;
  }
  model {
    dummy ~ normal(0, 1);
  }
  generated quantities {
    array[n_obs] int out_y_trans = y_obs_trans;
  }
  "

  y_obs <- c(10L, 25L, 0L)
  res <- run_stan_harness(code, data = list(n_obs = 3L, y_obs = y_obs))
  expect_equal(as.numeric(res$out_y_trans[1, ]), y_obs - 1L)
})
