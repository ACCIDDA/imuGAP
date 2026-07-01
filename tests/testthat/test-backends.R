# Backend selection + cross-backend vocabulary guard. The vocabulary logic is
# tested directly via assert_backend_vocab() so it runs without cmdstanr
# installed. cmdstanr is available on CI (fetched via the DESCRIPTION Remotes
# field), so the "when available" integration tests run there; they skip only
# where cmdstanr is genuinely absent. The not-installed guard is exercised by
# mocking requireNamespace, so it runs regardless of the environment.

test_that("assert_backend_vocab rejects the other backend's vocabulary", {
  expect_error(assert_backend_vocab("parallel_chains", "rstan"), "parallel_chains")
  expect_error(assert_backend_vocab("iter_warmup", "rstan"), "iter_warmup")
  expect_error(assert_backend_vocab("cores", "cmdstanr"), "cores")
  expect_error(assert_backend_vocab("control", "cmdstanr"), "control")
  expect_error(assert_backend_vocab("iter", "cmdstanr"), "iter")
})

test_that("assert_backend_vocab passes clean argument sets and returns them", {
  expect_identical(assert_backend_vocab(c("iter", "cores"), "rstan"), c("iter", "cores"))
  expect_silent(
    assert_backend_vocab(c("iter_warmup", "parallel_chains"), "cmdstanr")
  )
})

test_that("assert_backend_available validates and returns the backend", {
  expect_identical(assert_backend_available("rstan"), "rstan")
  expect_error(assert_backend_available("nonsense"), "should be one of")
})

test_that("stan_options() defaults to and records the rstan backend", {
  expect_identical(stan_options()$backend, "rstan")
  expect_error(stan_options(backend = "nonsense"))
})

test_that("rstan backend rejects cmdstanr vocabulary via stan_options", {
  expect_error(
    stan_options(backend = "rstan", parallel_chains = 4), "parallel_chains"
  )
})

test_that("stan_options errors for cmdstanr when it is not installed", {
  # Force cmdstanr to appear unavailable so the not-installed guard is exercised
  # even when cmdstanr *is* installed (as on CI, where it is fetched via the
  # DESCRIPTION Remotes field). Mocking base::requireNamespace keeps this test
  # meaningful regardless of the local environment.
  with_mocked_bindings(
    expect_error(
      stan_options(backend = "cmdstanr"),
      "requires the cmdstanr package"
    ),
    requireNamespace = function(...) FALSE,
    .package = "base"
  )
})

test_that("cmdstanr backend builds and records native options when available", {
  skip_if_not_installed("cmdstanr")
  opts <- stan_options(
    backend = "cmdstanr",
    parallel_chains = 4,
    iter_warmup = 500,
    iter_sampling = 500
  )
  expect_identical(opts$backend, "cmdstanr")
  expect_identical(opts$parallel_chains, 4L)
  expect_identical(opts$iter_warmup, 500L)
})

test_that("cmdstanr backend rejects rstan vocabulary when available", {
  skip_if_not_installed("cmdstanr")
  expect_error(stan_options(backend = "cmdstanr", cores = 4), "cores")
  expect_error(stan_options(backend = "cmdstanr", iter = 1000), "iter")
})

test_that("fit_model errors on an unknown backend", {
  # backend now rides on stan_opts; a bogus backend element must still be caught.
  expect_error(
    fit_model(
      "DUMMYMODEL",
      list(), NULL, list(backend = "nonsense"), NULL
    ),
    "should be one of"
  )
})

# The cmdstanr code paths can't run without the cmdstanr package and a CmdStan
# toolchain (absent on CI/CRAN). Mock the availability check, the cmdstanr fit,
# and rstan::sampling so the surrounding logic is still exercised.

test_that("stan_options validates the cmdstanr-native option set", {
  with_mocked_bindings(
    {
      opts <- stan_options(
        backend = "cmdstanr",
        iter_warmup = 500,
        iter_sampling = 250,
        parallel_chains = 2
      )
      expect_identical(opts$backend, "cmdstanr")
      expect_identical(opts$iter_warmup, 500L)
      expect_identical(opts$iter_sampling, 250L)
      expect_identical(opts$parallel_chains, 2L)
    },
    # Bypass the cmdstanr-installed check but keep match.arg validation.
    assert_backend_available = function(backend) {
      match.arg(backend, c("rstan", "cmdstanr"))
    }
  )
})

test_that("fit_model dispatches to the cmdstanr backend", {
  with_mocked_bindings(
    {
      res <- fit_model(
        "DUMMYMODEL",
        list(), NULL, stan_options(backend = "cmdstanr"), NULL
      )
      expect_identical(res, "cmdstanr_fit")
    },
    # fit_model() asserts availability first; bypass it so the dispatch (to the
    # mocked fit_cmdstanr) is what's exercised, with no cmdstanr installed.
    assert_backend_available = function(backend) invisible(backend),
    fit_cmdstanr = function(model_name, args, drop_pars = NULL) {
      "cmdstanr_fit"
    }
  )
})

test_that("assert_positive_int returns the coerced positive integer(s)", {
  expect_identical(assert_positive_int(4, "x"), 4L)
  expect_identical(assert_positive_int(c(1, 2, 3), "x"), c(1L, 2L, 3L))
  expect_type(assert_positive_int(2L, "x"), "integer")
})

test_that("assert_positive_int errors on invalid input", {
  expect_error(assert_positive_int("3", "x"), "numeric")
  expect_error(assert_positive_int(integer(0), "x"), ">= 1")
  expect_error(assert_positive_int(NA_integer_, "x"), "NA")
  expect_error(assert_positive_int(1.5, "x"), "integer")
  expect_error(assert_positive_int(0L, "x"), "positive")
  expect_error(assert_positive_int(-2L, "x"), "positive")
})

test_that("fit_rstan forwards drop_pars to rstan::sampling", {
  captured <- NULL
  with_mocked_bindings(
    {
      fit_rstan(
        "DUMMYMODEL",
        args = stan_options(chains = 1),
        drop_pars = "DUMMYPAR"
      )
      expect_identical(captured$pars, "DUMMYPAR")
      expect_false(captured$include)
    },
    sampling = function(...) {
      captured <<- list(...)
      "rstan_fit"
    },
    .package = "rstan"
  )
})
