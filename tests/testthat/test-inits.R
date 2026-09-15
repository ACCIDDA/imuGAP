# --- helpers for Stan data fixtures -------------------------------------------

make_test_stan_data <- function(
  n_doses = 2L,
  k_bs = 5L,
  n_layers = 1L,
  n_locs = 1L,
  n_parent_locs = 0L,
  y_obs_unmixed_uncensored = c(10L, 15L),
  y_smp_unmixed_uncensored = c(20L, 20L),
  y_obs_mixed_uncensored = integer(0),
  y_smp_mixed_uncensored = integer(0),
  y_obs_unmixed_right = integer(0),
  y_smp_unmixed_right = integer(0),
  y_obs_mixed_right = integer(0),
  y_smp_mixed_right = integer(0)
) {
  dat <- list(
    n_doses = n_doses,
    k_bs = k_bs,
    y_obs_unmixed_uncensored = y_obs_unmixed_uncensored,
    y_smp_unmixed_uncensored = y_smp_unmixed_uncensored,
    y_obs_mixed_uncensored = y_obs_mixed_uncensored,
    y_smp_mixed_uncensored = y_smp_mixed_uncensored,
    y_obs_unmixed_right = y_obs_unmixed_right,
    y_smp_unmixed_right = y_smp_unmixed_right,
    y_obs_mixed_right = y_obs_mixed_right,
    y_smp_mixed_right = y_smp_mixed_right
  )
  if (n_layers >= 2L) {
    dat$n_layers <- n_layers
    dat$n_locs <- n_locs
    dat$n_parent_locs <- n_parent_locs
  }
  dat
}

# --- tests -------------------------------------------------------------------

test_that("make_init_fn returns a 0-argument function returning initialized parameters", {
  dat <- make_test_stan_data()
  fn <- make_init_fn(dat)
  expect_true(is.function(fn))
  expect_equal(length(formals(fn)), 0L)

  inits <- fn()
  expect_type(inits, "list")
  expect_named(inits, c("lambda_raw", "beta_bs"), ignore.order = TRUE)
})

test_that("generate_inits handles single-layer data without layer parameters", {
  dat <- make_test_stan_data(n_doses = 3L, k_bs = 6L, n_layers = 1L)
  inits <- generate_inits(dat)

  expect_named(inits, c("lambda_raw", "beta_bs"), ignore.order = TRUE)
  expect_length(inits$lambda_raw, 3L)
  expect_length(inits$beta_bs, 6L)
  expect_null(inits$sigma_layer)
  expect_null(inits$z_layer)
  expect_true(all(is.finite(inits$lambda_raw)))
  expect_true(all(is.finite(inits$beta_bs)))
})

test_that("generate_inits handles multilayer hierarchy parameters", {
  dat <- make_test_stan_data(
    n_doses = 2L,
    k_bs = 4L,
    n_layers = 3L,
    n_locs = 5L,
    n_parent_locs = 2L
  )
  inits <- generate_inits(dat)

  expect_named(
    inits,
    c("lambda_raw", "sigma_layer", "z_layer", "beta_bs"),
    ignore.order = TRUE
  )
  expect_length(inits$lambda_raw, 2L)
  expect_length(inits$beta_bs, 4L)
  expect_length(inits$sigma_layer, 2L)
  expect_length(inits$z_layer, 2L)
  expect_true(all(inits$sigma_layer >= 0))
  expect_true(all(is.finite(inits$z_layer)))
})

test_that("generate_inits falls back to default coverage when observations are empty", {
  dat <- make_test_stan_data(
    y_obs_unmixed_uncensored = integer(0),
    y_smp_unmixed_uncensored = integer(0),
    y_obs_unmixed_right = integer(0),
    y_smp_unmixed_right = integer(0)
  )
  inits <- generate_inits(dat)
  expect_length(inits$beta_bs, dat$k_bs)
  # Default coverage of 0.85 maps to phi of 0.15
  expected_center <- stats::qlogis(0.15)
  expect_true(all(abs(inits$beta_bs - expected_center) < 0.5))
})

test_that("generate_inits falls back when total sample size is zero or results in NA/NaN", {
  dat_zero <- make_test_stan_data(
    y_obs_unmixed_uncensored = 0L,
    y_smp_unmixed_uncensored = 0L
  )
  inits_zero <- generate_inits(dat_zero)
  expected_center <- stats::qlogis(0.15)
  expect_true(all(abs(inits_zero$beta_bs - expected_center) < 0.5))

  dat_na <- make_test_stan_data(
    y_obs_unmixed_uncensored = as.integer(NA),
    y_smp_unmixed_uncensored = 10L
  )
  inits_na <- generate_inits(dat_na)
  expect_true(all(abs(inits_na$beta_bs - expected_center) < 0.5))
})

test_that("generate_inits clamps baseline phi to [0.01, 0.5] for extreme coverage", {
  # High coverage (100%) maps to phi clamped to lower bound 0.01
  dat_high <- make_test_stan_data(
    y_obs_unmixed_uncensored = 100L,
    y_smp_unmixed_uncensored = 100L
  )
  inits_high <- generate_inits(dat_high)
  expect_true(all(abs(inits_high$beta_bs - stats::qlogis(0.01)) < 0.5))

  # Low coverage (0%) maps to phi clamped to upper bound 0.5
  dat_low <- make_test_stan_data(
    y_obs_unmixed_uncensored = 0L,
    y_smp_unmixed_uncensored = 100L
  )
  inits_low <- generate_inits(dat_low)
  expect_true(all(abs(inits_low$beta_bs - stats::qlogis(0.5)) < 0.5))
})

test_that("generate_inits combines uncensored and right-censored observation data", {
  dat <- make_test_stan_data(
    y_obs_unmixed_uncensored = c(20L),
    y_smp_unmixed_uncensored = c(50L),
    y_obs_unmixed_right = c(40L),
    y_smp_unmixed_right = c(50L)
  )
  inits <- generate_inits(dat)
  # Combined sample gives 60% coverage, corresponding to phi of 0.40
  expected_center <- stats::qlogis(0.40)
  expect_true(all(abs(inits$beta_bs - expected_center) < 0.5))
})

test_that("generate_inits validates model argument", {
  dat <- make_test_stan_data()
  expect_error(
    generate_inits(dat, model = "unsupported_model"),
    "`imugap_opts` unknown model 'unsupported_model'"
  )
})
