
test_that("sampling and predict work correctly with simulated data", {
  data("locations_sim", package = "imuGAP")
  data("observations_sim", package = "imuGAP")
  data("populations_sim", package = "imuGAP")

  # Run estimation mode with minimal iterations for speed
  # Since the package has compiled Stan models, this should be fast.
  st_opts <- stan_options(chains = 1, iter = 10, warmup = 5, refresh = 0)
  imugap_opts <- imugap_options(df = 5, dose_schedule = c(1, 4))

  fit <- imuGAP::sampling(
    observations = observations_sim,
    populations = populations_sim,
    locations = locations_sim,
    imugap_opts = imugap_opts,
    stan_opts = st_opts
  )

  expect_s4_class(fit, "stanfit")

  # Verify transformed parameters (like logit_phi_st) are NOT in the fit
  # Since they were removed, they should not be present in the fitted parameters.
  fit_pars <- names(fit)
  expect_false("logit_phi_st" %in% fit_pars)
  expect_false("p_obs" %in% fit_pars)

  # Run prediction
  pred <- predict(
    fit = fit,
    populations = populations_sim,
    locations = locations_sim,
    imugap_opts = imugap_opts
  )

  expect_s3_class(pred, "data.table")
  expect_true(all(c("sample_id", "obs_id", "p_obs") %in% names(pred)))

  n_draws <- 5 # iter(10) - warmup(5)
  n_obs <- length(unique(populations_sim$obs_id))
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

  fit <- imuGAP::sampling(
    observations = observations_sim,
    populations = populations_sim,
    locations = locations_sim,
    imugap_opts = imugap_opts,
    stan_opts = st_opts
  )

  # Test county mismatch: modify locations to remove one school mapping
  bad_locations <- canonicalize_locations(locations_sim)
  bad_locations <- bad_locations[layer < 3 | loc_c_id != max(loc_c_id)]

  expect_error(
    predict(fit, populations_sim, bad_locations, imugap_opts),
    "Number of schools in 'locations'.*does not match"
  )

  # Test dose schedule mismatch
  bad_imugap_opts <- imugap_options(df = 5, dose_schedule = c(1, 2, 4))
  expect_error(
    predict(fit, populations_sim, locations_sim, bad_imugap_opts),
    "Dose schedule mismatch"
  )

  # Test B-spline specification mismatch
  bad_df_opts <- imugap_options(df = 6, dose_schedule = c(1, 4))
  expect_error(
    predict(fit, populations_sim, locations_sim, bad_df_opts),
    "B-spline degrees of freedom / specification mismatch"
  )
})
