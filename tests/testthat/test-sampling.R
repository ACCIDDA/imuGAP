# --- helpers -----------------------------------------------------------------

make_3layer_locs <- function() {
  data.frame(
    loc_id = c("state", "cnty1", "cnty2", "schlA", "schlB"),
    parent_id = c(NA, "state", "state", "cnty1", "cnty2")
  )
}

make_2layer_locs <- function() {
  data.frame(
    loc_id = c("state", "cnty1", "cnty2"),
    parent_id = c(NA, "state", "state")
  )
}

make_minimal_obs <- function() {
  data.frame(
    obs_id = c("o1", "o2"),
    positive = c(5L, 10L),
    sample_n = c(10L, 20L)
  )
}

make_minimal_pops <- function() {
  data.frame(
    obs_id = c("o1", "o2"),
    loc_id = c("schlA", "schlB"),
    cohort = c(1L, 1L),
    age = c(5L, 5L),
    dose = c(1L, 2L),
    weight = c(1.0, 1.0)
  )
}

# --- error paths -------------------------------------------------------------

test_that("sampling errors when location hierarchy has fewer than 3 layers", {
  expect_error(
    imuGAP::sampling(
      observations = make_minimal_obs(),
      populations = make_minimal_pops(),
      locations = make_2layer_locs()
    ),
    "3-layer"
  )
})

test_that("sampling errors when location hierarchy has more than 3 layers", {
  locs4 <- data.frame(
    loc_id = c("state", "cnty", "schl", "subschl"),
    parent_id = c(NA, "state", "cnty", "schl")
  )
  pops4 <- make_minimal_pops()
  pops4$loc_id <- c("subschl", "subschl")
  expect_error(
    imuGAP::sampling(
      observations = make_minimal_obs(),
      populations = pops4,
      locations = locs4
    ),
    "3-layer"
  )
})

test_that("sampling propagates validation errors from canonicalize_observations", {
  bad_obs <- data.frame(
    obs_id = c("o1", "o2"),
    positive = c(5L, 10L)
  )
  expect_error(
    imuGAP::sampling(
      observations = bad_obs,
      populations = make_minimal_pops(),
      locations = make_3layer_locs()
    ),
    "sample_n"
  )
})

test_that("sampling propagates validation errors from canonicalize_populations", {
  bad_pops <- make_minimal_pops()
  bad_pops$dose <- c(1L, 99L)
  expect_error(
    imuGAP::sampling(
      observations = make_minimal_obs(),
      populations = bad_pops,
      locations = make_3layer_locs()
    ),
    "dose"
  )
})

# --- data assembly path ------------------------------------------------------
#
# Mock rstan::sampling with with_mocked_bindings so imuGAP::sampling() runs the
# assembly pipeline but the mock captures stan_opts in lieu of sampling.

with_captured_sampling <- function(code) {
  captured_env <- new.env()
  captured_env$captured <- NULL
  fake <- function(...) {
    captured_env$captured <- list(...)
    structure(list(), class = "stanfit_mock")
  }
  testthat::with_mocked_bindings(
    {
      result <- force(code)
      list(result = result, captured = captured_env$captured)
    },
    sampling = fake,
    .package = "rstan"
  )
}

test_that("sampling() raises imugap_no_draws when the sampler produces no draws", {
  # rstan returns an empty mode-2 stanfit on failed init; imuGAP must not pass it
  # through silently (#107). new("stanfit") is a real S4 stanfit with an empty
  # @sim, matching what a failed initialization produces.
  empty_fit <- methods::new("stanfit")
  expect_error(
    with_mocked_bindings(
      suppressWarnings(imuGAP::sampling(
        observations = make_minimal_obs(),
        populations = make_minimal_pops(),
        locations = make_3layer_locs()
      )),
      sampling = function(...) empty_fit,
      .package = "rstan"
    ),
    class = "imugap_no_draws"
  )
})

test_that("sampling assembles stan_opts$data with all expected fields", {
  out <- with_captured_sampling(suppressWarnings(
    imuGAP::sampling(
      observations = make_minimal_obs(),
      populations = make_minimal_pops(),
      locations = make_3layer_locs()
    )
  ))
  expect_s3_class(out$result, "imugap_fit")
  expect_s3_class(out$result$stanfit, "stanfit_mock")
  d <- out$captured$data
  expect_true(is.list(d))
  expected_fields <- c(
    "n_uncensored_obs",
    "n_yr",
    "n_cohort",
    "n_sch",
    "n_doses",
    "dose_sched",
    "k_bs",
    "bs",
    "n_obs",
    "y_obs",
    "y_smp",
    "n_weights",
    "obs_to_weights_bounds",
    "weights_school",
    "weights_cohort",
    "weights_life_year",
    "weights_dose",
    "weights",
    "n_cnty",
    "cnty_bounds",
    "predict_mode"
  )
  expect_true(all(expected_fields %in% names(d)))
})

test_that("sampling data assembly produces sane derived values", {
  obs <- make_minimal_obs()
  pops <- make_minimal_pops()
  locs <- make_3layer_locs()
  opts <- imugap_options()
  out <- with_captured_sampling(suppressWarnings(
    imuGAP::sampling(observations = obs, populations = pops, locations = locs)
  ))
  d <- out$captured$data
  expect_equal(d$n_obs, nrow(obs))
  expect_equal(d$n_uncensored_obs, nrow(obs))
  expect_equal(d$n_doses, length(opts$dose_schedule))
  expect_equal(d$predict_mode, 0)
  n_layers <- canonicalize_locations(locs)[, .N, by = layer]
  expect_equal(d$n_cnty, n_layers[layer == 2, N])
  expect_equal(d$n_sch, n_layers[layer == 3, N])
  expect_equal(nrow(d$dose_sched), d$n_yr)
  expect_equal(ncol(d$dose_sched), d$n_doses)
})

test_that("sampling forwards observation positive/sample_n into stan data", {
  obs <- make_minimal_obs()
  out <- with_captured_sampling(suppressWarnings(
    imuGAP::sampling(
      observations = obs,
      populations = make_minimal_pops(),
      locations = make_3layer_locs()
    )
  ))
  d <- out$captured$data
  expect_setequal(d$y_obs, obs$positive)
  expect_setequal(d$y_smp, obs$sample_n)
})

test_that("sampling forwards object from imugap_opts to rstan::sampling", {
  out <- with_captured_sampling(suppressWarnings(
    imuGAP::sampling(
      observations = make_minimal_obs(),
      populations = make_minimal_pops(),
      locations = make_3layer_locs()
    )
  ))
  expect_s4_class(out$captured$object, "stanmodel")
  expect_equal(
    out$captured$object@model_name,
    "impute_school_coverage_process_v6"
  )
})

test_that("sampling forwards extra stan_opts (e.g. iter, chains)", {
  out <- with_captured_sampling(suppressWarnings(
    imuGAP::sampling(
      observations = make_minimal_obs(),
      populations = make_minimal_pops(),
      locations = make_3layer_locs(),
      stan_opts = stan_options(iter = 100, chains = 1, refresh = 0)
    )
  ))
  expect_equal(out$captured$iter, 100)
  expect_equal(out$captured$chains, 1)
  expect_equal(out$captured$refresh, 0)
})

test_that("sampling returns a structured imugap_fit object", {
  out <- with_captured_sampling(suppressWarnings(
    imuGAP::sampling(
      observations = make_minimal_obs(),
      populations = make_minimal_pops(),
      locations = make_3layer_locs()
    )
  ))
  fit <- out$result
  expect_s3_class(fit, "imugap_fit")
  expect_named(fit, c("stanfit", "settings", "data", "locations"))
  expect_s3_class(fit$locations, "data.table")
  expect_type(fit$settings, "list")
  expect_named(fit$settings, c("imugap_opts", "stan_opts"))
})
