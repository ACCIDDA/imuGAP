test_that("canonicalize_stan_options validates and normalizes stan_options", {
  st_opts <- stan_options()
  res <- canonicalize_stan_options(st_opts)

  expect_s3_class(res, "flexstan_options")
  expect_type(res$threads_per_chain, "integer")
  expect_equal(res$threads_per_chain, 1L)
  expect_equal(res$backend, "rstan")
})

test_that("canonicalize_stan_options preserves explicit threads_per_chain", {
  st_opts <- stan_options(threading = TRUE)
  res <- canonicalize_stan_options(st_opts)

  expect_s3_class(res, "flexstan_options")
  expect_equal(res$threads_per_chain, as.integer(st_opts$threads_per_chain))
  expect_gte(res$threads_per_chain, 1L)

  custom_opts <- list(backend = "cmdstanr", threads_per_chain = 4.0)
  res_custom <- canonicalize_stan_options(custom_opts)
  expect_s3_class(res_custom, "flexstan_options")
  expect_type(res_custom$threads_per_chain, "integer")
  expect_equal(res_custom$threads_per_chain, 4L)
})

test_that("canonicalize_stan_options accepts existing compliant S3 classes", {
  # Simulates future flexstanr version adding native S3 classes
  fake_flexstan <- structure(
    list(backend = "rstan", threads_per_chain = 2L),
    class = "flexstan_options"
  )
  res1 <- canonicalize_stan_options(fake_flexstan)
  expect_s3_class(res1, "flexstan_options")
  expect_equal(res1$threads_per_chain, 2L)

  fake_flexstanr <- structure(
    list(backend = "cmdstanr", threads_per_chain = 3L),
    class = "flexstanr_options"
  )
  res2 <- canonicalize_stan_options(fake_flexstanr)
  expect_s3_class(res2, "flexstan_options")
  expect_s3_class(res2, "flexstanr_options")
  expect_equal(res2$threads_per_chain, 3L)

  fake_stan_options <- structure(
    list(backend = "rstan"),
    class = "stan_options"
  )
  res3 <- canonicalize_stan_options(fake_stan_options)
  expect_s3_class(res3, "flexstan_options")
  expect_s3_class(res3, "stan_options")
  expect_equal(res3$threads_per_chain, 1L)
})

test_that("canonicalize_stan_options rejects non-compliant inputs", {
  # Hand-built list without backend
  expect_error(
    canonicalize_stan_options(list(chains = 1)),
    err_pattern(ERR_STAN_OPTS_CLASS)
  )

  # Non-list objects
  expect_error(
    canonicalize_stan_options("invalid"),
    err_pattern(ERR_STAN_OPTS_CLASS)
  )

  expect_error(
    canonicalize_stan_options(NULL),
    err_pattern(ERR_STAN_OPTS_CLASS)
  )

  expect_error(
    canonicalize_stan_options(123L),
    err_pattern(ERR_STAN_OPTS_CLASS)
  )
})
