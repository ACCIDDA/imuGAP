# --- helpers -----------------------------------------------------------------

make_3layer_locs <- function() {
  data.frame(
    loc_id = c("state", "cnty1", "cnty2", "schlA", "schlB"),
    parent_id = c(NA, "state", "state", "cnty1", "cnty1")
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

  # Incompatible dose schedule via imugap_opts (schedule requires 3 doses, but pops only has 2)
  expect_error(
    imuGAP::sampling(
      observations = make_minimal_obs(),
      populations = make_minimal_pops(),
      locations = make_3layer_locs(),
      imugap_opts = imugap_options(dose_schedule = c(1, 4, 7))
    ),
    err_pattern(ERR_DOSE_FINAL_NOT_OBSERVED, n_doses = 3L)
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
  expect_s3_class(out$result$raw_fit, "stanfit_mock")
  d <- out$captured$data
  expect_true(is.list(d))
  expected_fields <- c(
    "n_yr",
    "n_cohort",
    "n_locs",
    "n_layers",
    "layer_starts",
    "n_parent_locs",
    "parent_loc_id",
    "parent_child_starts",
    "n_doses",
    "n_intervals",
    "dt_vec",
    "dose_sched",
    "age_to_interval_map",
    "k_bs",
    "bs",
    "n_obs_unmixed_uncensored",
    "y_obs_unmixed_uncensored",
    "y_smp_unmixed_uncensored",
    "w_cohort_unmixed_uncensored",
    "w_age_unmixed_uncensored",
    "w_dose_unmixed_uncensored",
    "w_loc_unmixed_uncensored",
    "n_obs_mixed_uncensored",
    "y_obs_mixed_uncensored",
    "y_smp_mixed_uncensored",
    "n_weights_mixed_uncensored",
    "obs_bounds_mixed_uncensored",
    "w_cohort_mixed_uncensored",
    "w_age_mixed_uncensored",
    "w_dose_mixed_uncensored",
    "w_loc_mixed_uncensored",
    "weights_mixed_uncensored",
    "n_obs_unmixed_right",
    "y_obs_unmixed_right",
    "y_smp_unmixed_right",
    "w_cohort_unmixed_right",
    "w_age_unmixed_right",
    "w_dose_unmixed_right",
    "w_loc_unmixed_right",
    "n_obs_mixed_right",
    "y_obs_mixed_right",
    "y_smp_mixed_right",
    "n_weights_mixed_right",
    "obs_bounds_mixed_right",
    "w_cohort_mixed_right",
    "w_age_mixed_right",
    "w_dose_mixed_right",
    "w_loc_mixed_right",
    "weights_mixed_right",
    "n_obs_unmixed_left",
    "y_obs_unmixed_left",
    "y_smp_unmixed_left",
    "w_cohort_unmixed_left",
    "w_age_unmixed_left",
    "w_dose_unmixed_left",
    "w_loc_unmixed_left",
    "n_obs_mixed_left",
    "y_obs_mixed_left",
    "y_smp_mixed_left",
    "n_weights_mixed_left",
    "obs_bounds_mixed_left",
    "w_cohort_mixed_left",
    "w_age_mixed_left",
    "w_dose_mixed_left",
    "w_loc_mixed_left",
    "weights_mixed_left",
    "predict_mode",
    "num_threads"
  )
  expect_true(all(expected_fields %in% names(d)))
})

test_that("sampling data assembly produces sane derived values", {
  obs <- make_minimal_obs()
  pops <- make_minimal_pops()
  locs <- make_3layer_locs()
  opts <- imuGAP::imugap_options()
  default_threads <- flexstanr::stan_options(
    threading = TRUE
  )$threads_per_chain %||%
    1L
  out <- with_captured_sampling(suppressWarnings(
    imuGAP::sampling(observations = obs, populations = pops, locations = locs)
  ))
  d <- out$captured$data
  expect_equal(d$n_obs_unmixed_uncensored + d$n_obs_mixed_uncensored, nrow(obs))
  expect_equal(d$n_obs_unmixed_right + d$n_obs_mixed_right, 0L)
  expect_equal(d$n_obs_unmixed_left + d$n_obs_mixed_left, 0L)
  expect_equal(d$n_doses, length(opts$dose_schedule))
  expect_equal(d$predict_mode, 0)
  expect_equal(d$num_threads, default_threads)
  expect_gte(d$num_threads, 1L)
  expect_equal(d$n_locs, 5L)
  expect_equal(d$n_layers, 3L)
  expect_equal(as.integer(d$layer_starts), c(1L, 2L, 4L))
  expect_equal(as.integer(d$parent_child_starts), c(2L, 4L))
  expect_equal(nrow(d$dose_sched), d$n_intervals)
  expect_equal(ncol(d$dose_sched), d$n_doses)
})

test_that("sampling respects explicit single-threaded and multi-threaded options", {
  obs <- make_minimal_obs()
  pops <- make_minimal_pops()
  locs <- make_3layer_locs()

  out_single <- with_captured_sampling(suppressWarnings(
    imuGAP::sampling(
      observations = obs,
      populations = pops,
      locations = locs,
      stan_opts = flexstanr::stan_options(threading = FALSE)
    )
  ))
  expect_equal(out_single$captured$data$num_threads, 1L)
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
  expect_setequal(
    c(d$y_obs_unmixed_uncensored, d$y_obs_mixed_uncensored),
    obs$positive
  )
  expect_setequal(
    c(d$y_smp_unmixed_uncensored, d$y_smp_mixed_uncensored),
    obs$sample_n
  )
})

test_that("sampling translates model from imugap_opts and hierarchy depth to stan model", {
  # Multi-layer hierarchy dispatches to multi-layer Stan model
  out_multi <- with_captured_sampling(suppressWarnings(
    imuGAP::sampling(
      observations = make_minimal_obs(),
      populations = make_minimal_pops(),
      locations = make_3layer_locs()
    )
  ))
  expect_s4_class(out_multi$captured$object, "stanmodel")
  expect_equal(
    out_multi$captured$object@model_name,
    "impute_school_coverage_process_v6"
  )

  # 1-layer hierarchy dispatches to specialized single-layer Stan model
  locs1 <- data.frame(loc_id = "state", parent_id = NA)
  pops1 <- data.frame(
    obs_id = c("o1", "o2"),
    loc_id = c("state", "state"),
    cohort = c(1L, 1L),
    age = c(5L, 5L),
    dose = c(1L, 2L),
    weight = c(1.0, 1.0)
  )
  out_single <- with_captured_sampling(suppressWarnings(
    imuGAP::sampling(
      observations = make_minimal_obs(),
      populations = pops1,
      locations = locs1
    )
  ))
  expect_s4_class(out_single$captured$object, "stanmodel")
  expect_equal(
    out_single$captured$object@model_name,
    "impute_school_coverage_process_v6_single_layer"
  )
})

test_that("sampling forwards extra stan_opts (e.g. iter, chains)", {
  out <- with_captured_sampling(suppressWarnings(
    imuGAP::sampling(
      observations = make_minimal_obs(),
      populations = make_minimal_pops(),
      locations = make_3layer_locs(),
      stan_opts = flexstanr::stan_options(iter = 100, chains = 1, refresh = 0)
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
  expect_named(
    fit,
    c("raw_fit", "settings", "data", "locations"),
    ignore.order = TRUE
  )
  expect_s3_class(fit$locations, "data.table")
  expect_named(
    fit$settings,
    c("imugap_opts", "stan_opts"),
    ignore.order = TRUE
  )
  expect_named(
    fit$settings$imugap_opts,
    c("df", "dose_schedule", "model", "model_name"),
    ignore.order = TRUE
  )
})

test_that("sampling errors when stan_opts was not built by stan_options()", {
  # A hand-built list carries no backend tag; sampling() must reject it before
  # reaching the sampler (so this needs no Stan compilation).
  expect_error(
    suppressWarnings(imuGAP::sampling(
      observations = make_minimal_obs(),
      populations = make_minimal_pops(),
      locations = make_3layer_locs(),
      stan_opts = list(chains = 1)
    )),
    "stan_options"
  )
})

test_that("sampling errors when imugap_opts contains an unknown model", {
  expect_error(
    imuGAP::sampling(
      observations = make_minimal_obs(),
      populations = make_minimal_pops(),
      locations = make_3layer_locs(),
      imugap_opts = list(model = "unsupported_model")
    ),
    "unknown model 'unsupported_model'"
  )
})
