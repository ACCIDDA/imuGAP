test_that("sampling and predict work correctly with simulated data", {
  data("locations_sim", package = "imuGAP")
  data("observations_sim", package = "imuGAP")
  data("populations_sim", package = "imuGAP")

  # Run estimation mode with minimal iterations for speed
  # Since the package has compiled Stan models, this should be fast.
  st_opts <- stan_options(chains = 1, iter = 10, warmup = 5, refresh = 0)
  imugap_opts <- imugap_options(df = 5, dose_schedule = c(1, 4))

  fit <- suppressWarnings(imuGAP::sampling(
    observations = observations_sim,
    populations = populations_sim,
    locations = locations_sim,
    imugap_opts = imugap_opts,
    stan_opts = st_opts
  ))

  expect_s3_class(fit, "imugap_fit")
  expect_s4_class(fit$stanfit, "stanfit")

  # Verify transformed parameters (like logit_phi_st) are NOT in the fit
  # Since they were removed, they should not be present in the fitted parameters.
  fit_pars <- names(fit$stanfit)
  expect_false("logit_phi_st" %in% fit_pars)
  expect_false("p_obs" %in% fit_pars)

  # Run prediction
  clean_pops <- data.table::copy(populations_sim)
  clean_pops$weight <- 1.0
  clean_pops <- clean_pops[!duplicated(clean_pops$obs_id), ]

  pred <- predict(
    fit,
    clean_pops
  )

  expect_s3_class(pred, "data.table")
  expect_true(all(c("sample_id", "obs_id", "p_obs") %in% names(pred)))

  n_draws <- 5 # 10 iterations minus 5 warmup
  n_obs <- length(unique(clean_pops$obs_id))
  expect_equal(nrow(pred), n_draws * n_obs)

  # Check value range of p_obs
  expect_true(all(pred$p_obs >= 0 & pred$p_obs <= 1))
})

test_that("predict throws informative compatibility errors", {
  data("locations_sim", package = "imuGAP")
  data("observations_sim", package = "imuGAP")
  data("populations_sim", package = "imuGAP")

  st_opts <- stan_options(chains = 1, iter = 10, warmup = 5, refresh = 0)
  imugap_opts <- imugap_options(df = 5, dose_schedule = c(1, 4))

  fit <- suppressWarnings(imuGAP::sampling(
    observations = observations_sim,
    populations = populations_sim,
    locations = locations_sim,
    imugap_opts = imugap_opts,
    stan_opts = st_opts
  ))

  clean_pops <- data.table::copy(populations_sim)
  clean_pops$weight <- 1.0
  clean_pops <- clean_pops[!duplicated(clean_pops$obs_id), ]

  # Test wrong class of object
  expect_error(
    predict("not_a_fit", clean_pops),
    "no applicable method for 'predict'"
  )

  # Test location mismatch
  bad_loc_pops <- data.table::copy(clean_pops)
  bad_loc_pops$loc_id[1] <- "unknown_loc"
  expect_error(
    predict(fit, bad_loc_pops),
    "all locations must be within fit\\$locations"
  )

  # Test dose bounds mismatch
  bad_dose_pops <- data.table::copy(clean_pops)
  bad_dose_pops$dose[1] <- 99L
  expect_error(
    predict(fit, bad_dose_pops),
    "dose values must be within 1 and fit\\$data\\$n_doses"
  )

  # Test age bounds mismatch
  bad_age_pops <- data.table::copy(clean_pops)
  bad_age_pops$age[1] <- 99L
  expect_error(
    predict(fit, bad_age_pops),
    "age values must be within 1 and fit\\$data\\$n_yr"
  )

  # Test cohort bounds mismatch
  bad_cohort_pops <- data.table::copy(clean_pops)
  bad_cohort_pops$cohort[1] <- 99L
  expect_error(
    predict(fit, bad_cohort_pops),
    "cohort values must be within 1 and fit\\$data\\$n_cohort"
  )
})
