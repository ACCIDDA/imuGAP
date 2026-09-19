test_that("print.imugap_fit returns object invisibly and outputs summary", {
  data("fit_sim", package = "imuGAP")

  out <- capture.output(res <- print(fit_sim))
  expect_identical(res, fit_sim)
  expect_true(any(grepl("An imuGAP model fit", out)))
  expect_true(any(grepl("Hierarchy:", out)))
  expect_true(any(grepl("Observations:", out)))
  expect_true(any(grepl("beta_bs\\[1\\]", out)))
  expect_true(any(grepl("sigma_layer\\[1\\]", out)))
  expect_true(any(grepl("sigma_layer\\[2\\]", out)))
  expect_false(any(grepl("sigma_layer\\[3\\]", out)))
  expect_true(any(grepl("lambda_raw\\[1\\]", out)))
  # z_layer offset parameters should be excluded by default
  expect_false(any(grepl("z_layer\\[1\\]", out)))
})

test_that("print.imugap_fit works on 1-layer and 2-layer fits", {
  data("fit_sim_1layer", package = "imuGAP")
  data("fit_sim_2layer", package = "imuGAP")

  out_1 <- capture.output(res_1 <- print(fit_sim_1layer))
  expect_identical(res_1, fit_sim_1layer)
  expect_true(any(grepl("Hierarchy:.*1 locations across 1 layer", out_1)))
  expect_true(any(grepl("beta_bs\\[1\\]", out_1)))
  expect_false(any(grepl("sigma_layer", out_1)))
  expect_false(any(grepl("z_layer", out_1)))

  out_2 <- capture.output(res_2 <- print(fit_sim_2layer))
  expect_identical(res_2, fit_sim_2layer)
  expect_true(any(grepl("Hierarchy:.*4 locations across 2 layers", out_2)))
  expect_true(any(grepl("sigma_layer\\[1\\]", out_2)))
  expect_false(any(grepl("sigma_layer\\[2\\]", out_2)))
  expect_false(any(grepl("z_layer", out_2)))
})

test_that("print.imugap_fit respects custom pars argument", {
  data("fit_sim", package = "imuGAP")

  out <- capture.output(print(fit_sim, pars = "lambda_raw"))
  expect_true(any(grepl("lambda_raw\\[1\\]", out)))
  expect_false(any(grepl("beta_bs\\[1\\]", out)))
})

test_that("print.imugap_fit errors on invalid object", {
  expect_error(
    print.imugap_fit(list(a = 1)),
    "`x` must be an object of class 'imugap_fit'"
  )
})

test_that("print.imugap_predict returns object invisibly and outputs summary", {
  data("predict_sim", package = "imuGAP")

  out <- capture.output(res <- print(predict_sim))
  expect_identical(res, predict_sim)
  expect_true(any(grepl("An imuGAP predictions object", out)))
  expect_true(any(grepl("Targets:", out)))
  expect_true(any(grepl("Posterior:", out)))
  expect_true(any(grepl("Use summary\\(\\)", out)))
})

test_that("print.imugap_predict errors on invalid object", {
  expect_error(
    print.imugap_predict(list(a = 1)),
    "`x` must be an object of class 'imugap_predict'"
  )
})

test_that("print.imugap_fit handles missing locations, data, and raw_fit variants", {
  # Fit with NULL locations and data
  mock_fit1 <- structure(list(), class = "imugap_fit")
  out1 <- capture.output(res1 <- print(mock_fit1))
  expect_identical(res1, mock_fit1)
  expect_true(any(grepl("An imuGAP model fit", out1)))
  expect_false(any(grepl("Hierarchy:", out1)))
  expect_false(any(grepl("Observations:", out1)))

  # Fit with custom non-stanfit backend object
  mock_raw <- structure(list(), class = "mock_backend_fit")
  mock_fit2 <- structure(list(raw_fit = mock_raw), class = "imugap_fit")
  out2 <- capture.output(print(mock_fit2))
  expect_true(any(grepl("An imuGAP model fit", out2)))
})

test_that("print.imugap_predict handles 2D matrix, 1D vector, and missing metadata", {
  # 2D draws matrix
  draws_2d <- matrix(runif(50 * 2), nrow = 50, ncol = 2)
  pred_2d <- structure(list(draws = draws_2d), class = "imugap_predict")
  out_2d <- capture.output(print(pred_2d))
  expect_true(any(grepl("Posterior: 50 draws", out_2d)))

  # 1D draws vector
  draws_1d <- runif(25)
  pred_1d <- structure(list(draws = draws_1d), class = "imugap_predict")
  out_1d <- capture.output(print(pred_1d))
  expect_true(any(grepl("Posterior: 25 draws", out_1d)))

  # Target without loc_id
  target_noloc <- data.table::data.table(age = 5L, cohort = 1L, dose = 1L)
  pred_noloc <- structure(
    list(draws = array(runif(10), dim = c(5, 2, 1)), target = target_noloc),
    class = "imugap_predict"
  )
  out_noloc <- capture.output(print(pred_noloc))
  expect_true(any(grepl("Targets:   1 target population slice", out_noloc)))
})

test_that("as.data.frame.imugap_predict errors on non-predict object", {
  expect_error(
    as.data.frame.imugap_predict(list(a = 1)),
    "`x` must be an object of class 'imugap_predict'"
  )
})

test_that("subset.imugap_predict validates inputs and expressions", {
  expect_error(
    subset.imugap_predict(list(a = 1)),
    "`x` must be an object of class 'imugap_predict'"
  )

  data("predict_sim", package = "imuGAP")
  expect_error(
    subset(predict_sim, "invalid_non_logical_expression"),
    "must be a logical vector"
  )
})

test_that("print.imugap_fit dispatches to CmdStanMCMC print with variables", {
  printed_vars <- NULL
  mock_cmdstan <- list(
    metadata = function() {
      list(stan_variables = c("beta_bs", "z_layer", "lambda_raw"))
    },
    print = function(variables = NULL, ...) {
      printed_vars <<- variables
      cat("mock CmdStanMCMC print\n")
    }
  )
  class(mock_cmdstan) <- "CmdStanMCMC"
  fit_mock <- structure(list(raw_fit = mock_cmdstan), class = "imugap_fit")

  out <- capture.output(print(fit_mock))
  expect_equal(printed_vars, c("beta_bs", "lambda_raw"))
  expect_true(any(grepl("mock CmdStanMCMC print", out)))

  # Explicit pars
  capture.output(print(fit_mock, pars = "custom_par"))
  expect_equal(printed_vars, "custom_par")
})
