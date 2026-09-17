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
    "`fit` must be an object of class 'imugap_fit'"
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
