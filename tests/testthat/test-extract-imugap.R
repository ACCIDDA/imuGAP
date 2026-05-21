# Tests for extract_imugap()

test_that("extract_imugap errors on non-imugap_fit input", {
  expect_error(extract_imugap(list()))
  expect_error(extract_imugap(NULL))
  expect_error(extract_imugap("not a stanfit"))
  expect_error(extract_imugap(data.frame(x = 1)))
})

test_that("extract_imugap extracts from a valid imugap_fit", {
  raw_fit <- structure(list(par_dims = list(logit_phi_st = 1)), class = "stanfit")
  testthat::with_mocked_bindings(
    {
      fit <- structure(
        list(
          stanfit = raw_fit,
          settings = list(),
          data = list(),
          algorithm = "MCMC"
        ),
        class = "imugap_fit"
      )
      res <- extract_imugap(fit, pars = "logit_phi_st")
      expect_equal(res, "mocked_extracted_value")
    },
    extract = function(object, pars, ...) {
      expect_true(inherits(object, "stanfit"))
      expect_equal(pars, "logit_phi_st")
      "mocked_extracted_value"
    },
    .package = "rstan"
  )
})
