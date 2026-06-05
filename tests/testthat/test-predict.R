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
  expect_true(is.array(pred$draws))
  expect_s3_class(pred$target, "data.table")

  chains <- if (!is.null(st_opts$chains)) st_opts$chains else 4L
  iter <- if (!is.null(st_opts$iter)) st_opts$iter else 2000L
  warmup <- if (!is.null(st_opts$warmup)) st_opts$warmup else (iter %/% 2L)
  n_obs <- length(unique(clean_pops$obs_id))

  dims <- dim(pred$draws)
  expect_equal(length(dims), 3L)
  expect_equal(dims[1], (iter - warmup) * chains)
  expect_equal(dims[2], 1L)
  expect_equal(dims[3], n_obs)

  # Check value range of draws
  expect_true(all(pred$draws >= 0 & pred$draws <= 1))

  # Test summary method
  sum_df <- summary(pred)
  expect_s3_class(sum_df, "data.table")
  expect_true(all(c("mean", "q2_5", "q50", "q97_5") %in% names(sum_df)))
  expect_equal(nrow(sum_df), n_obs)
  expect_true(all(sum_df$mean >= 0 & sum_df$mean <= 1))

  # Test summary method with custom quantiles
  sum_df2 <- summary(pred, probs = c(0.1, 0.9))
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

test_that("subset.imugap_predict subsets draws and metadata correctly", {
  data("predict_sim", package = "imuGAP")

  # Original dimensions: 2000 iterations, 1 chain, 1008 variables
  orig_dims <- dim(predict_sim$draws)
  expect_equal(orig_dims[1], 2000)
  expect_equal(orig_dims[2], 1)
  expect_equal(orig_dims[3], 1008)

  # Check subsetting by metadata expression
  pred_sub1 <- subset(predict_sim, dose == 2)
  expect_s3_class(pred_sub1, "imugap_predict")
  expect_equal(dim(pred_sub1$draws)[1], 2000)
  expect_equal(dim(pred_sub1$draws)[2], 1)
  expect_equal(dim(pred_sub1$draws)[3], sum(predict_sim$target$dose == 2))
  expect_equal(nrow(pred_sub1$target), sum(predict_sim$target$dose == 2))
  expect_true(all(pred_sub1$target$dose == 2))

  # Check subsetting by iteration
  pred_sub2 <- subset(predict_sim, iteration = 1:100)
  expect_equal(dim(pred_sub2$draws)[1], 100)
  expect_equal(dim(pred_sub2$draws)[2], 1)
  expect_equal(dim(pred_sub2$draws)[3], 1008)

  # Check subsetting by chain
  pred_sub3 <- subset(predict_sim, chain = 1)
  expect_equal(dim(pred_sub3$draws)[1], 2000)
  expect_equal(dim(pred_sub3$draws)[2], 1)
  expect_equal(dim(pred_sub3$draws)[3], 1008)

  # Check subsetting by both metadata and iterations/chains
  pred_sub4 <- subset(predict_sim, dose == 2, iteration = 1:5, chain = 1)
  expect_equal(dim(pred_sub4$draws)[1], 5)
  expect_equal(dim(pred_sub4$draws)[2], 1)
  expect_equal(dim(pred_sub4$draws)[3], sum(predict_sim$target$dose == 2))
  expect_equal(nrow(pred_sub4$target), sum(predict_sim$target$dose == 2))
})

test_that("as.data.frame.imugap_predict works correctly", {
  data("predict_sim", package = "imuGAP")

  # Full object
  df_full <- as.data.frame(predict_sim)
  expect_s3_class(df_full, "data.table")
  expect_s3_class(df_full, "data.frame")

  dims <- dim(predict_sim$draws)
  dim_i <- dims[1]
  dim_c <- dims[2]
  dim_v <- dims[3]
  expected_rows <- dim_i * dim_c * dim_v

  expect_equal(nrow(df_full), expected_rows)
  expect_true(all(c("iteration", "chain", "coverage") %in% colnames(df_full)))

  # Verify target columns are in df_full
  target_cols <- colnames(predict_sim$target)
  expect_true(all(target_cols %in% colnames(df_full)))

  # Verify values mapping for first few iterations/chains/variables
  expect_equal(df_full$coverage[1], predict_sim$draws[1, 1, 1])
  expect_equal(df_full$coverage[2], predict_sim$draws[2, 1, 1])
  expect_equal(df_full$coverage[dim_i], predict_sim$draws[dim_i, 1, 1])
  expect_equal(df_full$coverage[dim_i + 1], predict_sim$draws[1, 1, 2]) # since C = 1 in predict_sim

  # Test with a subsetted view
  pred_sub <- subset(predict_sim, dose == 2, iteration = 1:5)
  df_sub <- as.data.frame(pred_sub)

  sub_dims <- dim(pred_sub$draws)
  expected_sub_rows <- prod(sub_dims)

  expect_s3_class(df_sub, "data.table")
  expect_s3_class(df_sub, "data.frame")
  expect_equal(nrow(df_sub), expected_sub_rows)
  expect_true(all(df_sub$dose == 2))
  expect_true(all(df_sub$iteration %in% 1:5))
  expect_equal(df_sub$coverage[1], pred_sub$draws[1, 1, 1])
  expect_equal(df_sub$coverage[5], pred_sub$draws[5, 1, 1])
  expect_equal(df_sub$coverage[6], pred_sub$draws[1, 1, 2])
})
