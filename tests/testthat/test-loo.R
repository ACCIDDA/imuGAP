test_that("log_lik.imugap_fit computes valid pointwise log-likelihood matrix", {
  data("fit_sim", package = "imuGAP")

  # Test with sub-sampled posterior size
  ll_sub <- suppressWarnings(log_lik(fit_sim, posterior_size = 50))
  expect_true(is.matrix(ll_sub))
  expect_equal(nrow(ll_sub), 52L) # 50 rounded up to multiple of 4 chains = 52
  expect_true(ncol(ll_sub) > 0L)
  expect_true(all(is.finite(ll_sub)))
  expect_true(all(ll_sub <= 0))

  # Test with all draws
  ll_all <- log_lik(fit_sim)
  expect_true(is.matrix(ll_all))
  expect_equal(nrow(ll_all), 2000L) # fit_sim has 2000 posterior draws (500 iter x 4 chains)
  expect_equal(ncol(ll_all), ncol(ll_sub))
  expect_true(all(is.finite(ll_all)))
  expect_true(all(ll_all <= 0))
})

test_that("log_lik and loo raise appropriate errors for invalid inputs", {
  expect_error(
    log_lik("not_a_fit"),
    "no applicable method for 'log_lik'"
  )

  expect_error(
    loo("not_a_fit"),
    "no applicable method for 'loo'"
  )

  expect_error(
    log_lik.imugap_fit("not_a_fit"),
    err_pattern(ERR_NOT_IMUGAP_FIT, name = "object")
  )

  expect_error(
    loo.imugap_fit("not_a_fit"),
    err_pattern(ERR_NOT_IMUGAP_FIT, name = "x")
  )
})

test_that("loo.imugap_fit errors informatively when loo package is not available", {
  data("fit_sim", package = "imuGAP")
  testthat::with_mocked_bindings(
    expect_error(
      loo(fit_sim),
      err_pattern(ERR_LOO_NOT_INSTALLED)
    ),
    requireNamespace = function(package, ...) {
      if (package == "loo") FALSE else base::requireNamespace(package, ...)
    },
    .package = "base"
  )
})

test_that("loo.imugap_fit works with loo package", {
  skip_if_not_installed("loo")
  data("fit_sim", package = "imuGAP")

  loo_res <- suppressWarnings(loo(fit_sim, posterior_size = 100))
  expect_s3_class(loo_res, "loo")
  expect_true(all(
    c("estimates", "pointwise", "diagnostics") %in% names(loo_res)
  ))
  expect_true("elpd_loo" %in% rownames(loo_res$estimates))
  expect_true("p_loo" %in% rownames(loo_res$estimates))
  expect_true("looic" %in% rownames(loo_res$estimates))
})
