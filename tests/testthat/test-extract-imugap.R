# Tests for extract_imugap()
#
# These exercise extract_imugap() against the bundled `fit_sim` stanfit fixture
# so the default-args path is covered by real model output (regression guard for
# the historical `logit_phi_state` typo, which is not actually a parameter the
# Stan model emits) and so common explicit-`pars` combinations are sanity
# checked.

test_that("extract_imugap errors on non-stanfit input", {
  expect_error(extract_imugap(list()))
  expect_error(extract_imugap(NULL))
  expect_error(extract_imugap("not a stanfit"))
  expect_error(extract_imugap(data.frame(x = 1)))
})

test_that("extract_imugap default returns logit_phi_st with expected shape", {
  data("fit_sim")

  out <- extract_imugap(fit_sim)

  expect_type(out, "list")
  expect_named(out, "logit_phi_st")

  n_cohort <- fit_sim@par_dims[["logit_phi_st"]]
  # iter = 100 with default warmup = iter / 2 leaves 50 post-warmup samples
  expect_equal(dim(out$logit_phi_st), c(50L, n_cohort))
  expect_true(all(is.finite(out$logit_phi_st)))
})

test_that("extract_imugap(pars = 'phi') returns values in [0, 1]", {
  data("fit_sim")

  out <- extract_imugap(fit_sim, pars = "phi")

  expect_named(out, "phi")
  expect_true(all(is.finite(out$phi)))
  expect_true(all(out$phi >= 0 & out$phi <= 1))
})

test_that("extract_imugap(pars = 'lambda_raw') returns n_doses columns", {
  data("fit_sim")

  out <- extract_imugap(fit_sim, pars = "lambda_raw")

  expect_named(out, "lambda_raw")
  # lambda_raw is a vector of length n_doses per draw; rstan::extract returns
  # a (draws x n_doses) matrix in permuted = TRUE mode.
  n_doses <- length(imugap_options()$dose_schedule)
  expect_equal(ncol(out$lambda_raw), n_doses)
  expect_equal(nrow(out$lambda_raw), 50L)
})

test_that("extract_imugap forwards `...` to rstan::extract (permuted = FALSE)", {
  data("fit_sim")

  out <- extract_imugap(fit_sim, pars = "phi", permuted = FALSE)

  # permuted = FALSE returns an array (iterations x chains x parameters), not
  # a named list of per-parameter samples.
  expect_true(is.array(out))
  expect_false(is.list(out))
  expect_length(dim(out), 3L)
})
