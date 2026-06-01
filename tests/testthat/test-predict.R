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

  expect_s3_class(pred, "imugap_predict")
  expect_true(is.matrix(pred$draws))
  expect_s3_class(pred$target, "data.table")

  chains <- if (!is.null(st_opts$chains)) st_opts$chains else 4L
  iter <- if (!is.null(st_opts$iter)) st_opts$iter else 2000L
  warmup <- if (!is.null(st_opts$warmup)) st_opts$warmup else (iter %/% 2L)
  n_draws <- (iter - warmup) * chains
  n_obs <- length(unique(clean_pops$obs_id))
  expect_equal(nrow(pred$draws), n_draws)
  expect_equal(ncol(pred$draws), n_obs)

  # Check value range of draws
  expect_true(all(pred$draws >= 0 & pred$draws <= 1))

  # Test summary method
  sum_df <- summary(pred)
  expect_s3_class(sum_df, "data.table")
  expect_true(all(c("mean", "q2_5", "q50", "q97_5") %in% names(sum_df)))
  expect_equal(nrow(sum_df), n_obs)
  expect_true(all(sum_df$mean >= 0 & sum_df$mean <= 1))

  # Test summarize method with custom quantiles
  sum_df2 <- summarize(pred, probs = c(0.1, 0.9))
  expect_s3_class(sum_df2, "data.table")
  expect_true(all(c("mean", "q10", "q90") %in% names(sum_df2)))
  expect_equal(nrow(sum_df2), n_obs)
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
