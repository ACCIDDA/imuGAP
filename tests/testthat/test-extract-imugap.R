# Tests for extract_imugap()

test_that("extract_imugap errors on non-imugap_fit input", {
  expect_error(extract_imugap(list()))
  expect_error(extract_imugap(NULL))
  expect_error(extract_imugap("not a stanfit"))
  expect_error(extract_imugap(data.frame(x = 1)))
})

test_that("extract_imugap extracts from a valid imugap_fit", {
  raw_fit <- structure(
    list(par_dims = list(beta_bs = 1, lambda_raw = 1)),
    class = "stanfit"
  )
  testthat::with_mocked_bindings(
    {
      fit <- structure(
        list(
          raw_fit = raw_fit,
          settings = list(),
          data = list()
        ),
        class = "imugap_fit"
      )
      res <- extract_imugap(fit)
      expect_equal(res, "mocked_extracted_value")
    },
    extract = function(object, pars, ...) {
      expect_true(inherits(object, "stanfit"))
      expect_equal(pars, "beta_bs")
      "mocked_extracted_value"
    },
    .package = "rstan"
  )

  testthat::with_mocked_bindings(
    {
      fit <- structure(
        list(
          raw_fit = raw_fit,
          settings = list(),
          data = list()
        ),
        class = "imugap_fit"
      )
      res <- extract_imugap(fit, pars = "lambda_raw")
      expect_equal(res, "mocked_extracted_value_custom")
    },
    extract = function(object, pars, ...) {
      expect_true(inherits(object, "stanfit"))
      expect_equal(pars, "lambda_raw")
      "mocked_extracted_value_custom"
    },
    .package = "rstan"
  )
})

# CmdStanMCMC fits delegate to backend_extract().
test_that("extract_imugap() delegates to backend_extract for CmdStanMCMC fit", {
  fake_fit <- structure(
    list(raw_fit = structure(list(), class = "CmdStanMCMC")),
    class = "imugap_fit"
  )
  testthat::with_mocked_bindings(
    {
      res <- extract_imugap(fake_fit, pars = "beta_bs")
      expect_equal(res, "mocked_cmdstanr_extracted")
    },
    backend_extract = function(raw_fit, pars = NULL, ...) {
      expect_s3_class(raw_fit, "CmdStanMCMC")
      expect_equal(pars, "beta_bs")
      "mocked_cmdstanr_extracted"
    }
  )
})
