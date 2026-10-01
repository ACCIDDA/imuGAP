# Unit tests for subset_draws_tail helper in R/helpers.R

test_that("subset_draws_tail returns original array when posterior_size is NULL", {
  draws_array <- array(seq_len(60), dim = c(10, 2, 3))
  expect_identical(
    subset_draws_tail(draws_array, posterior_size = NULL),
    draws_array
  )
})

test_that("subset_draws_tail extracts converged tail when posterior_size is a multiple of chains", {
  # 10 iterations, 2 chains, 3 parameters = 20 draws available
  draws_array <- array(seq_len(60), dim = c(10, 2, 3))

  # Request 6 draws -> 3 per chain (iterations 8, 9, 10)
  res <- suppressWarnings(subset_draws_tail(draws_array, posterior_size = 6))

  expect_equal(dim(res), c(3, 2, 3))
  expect_identical(res, draws_array[8:10, , , drop = FALSE])
})

test_that("subset_draws_tail warns about adequacy when sub-sample is taken", {
  draws_array <- array(seq_len(60), dim = c(10, 2, 3))

  w <- testthat::capture_warnings(
    subset_draws_tail(draws_array, posterior_size = 6)
  )
  expect_match(
    w,
    err_pattern(MSG_POSTERIOR_SUBSAMPLE_WARN, posterior_size = 6),
    all = FALSE
  )
})

test_that("subset_draws_tail rounds up when posterior_size is not a multiple of chains", {
  draws_array <- array(seq_len(60), dim = c(10, 2, 3))

  # Request 5 draws with 2 chains -> rounded up to 6 with warning
  w <- testthat::capture_warnings(
    res <- subset_draws_tail(draws_array, posterior_size = 5)
  )

  expect_match(
    w,
    err_pattern(
      MSG_POSTERIOR_SIZE_ROUNDED,
      posterior_size = 5,
      n_chains = 2,
      adjusted_size = 6
    ),
    all = FALSE
  )
  expect_equal(dim(res), c(3, 2, 3))
  expect_identical(res, draws_array[8:10, , , drop = FALSE])
})

test_that("subset_draws_tail errors on invalid inputs", {
  draws_array <- array(seq_len(60), dim = c(10, 2, 3))

  # Non-positive or non-integer
  expect_error(subset_draws_tail(draws_array, posterior_size = 0), "positive")
  expect_error(subset_draws_tail(draws_array, posterior_size = 2.5), "integer")
  expect_error(
    subset_draws_tail(draws_array, posterior_size = c(2, 4)),
    err_pattern(ERR_POSTERIOR_SIZE_SINGLE)
  )

  # Exceeds available draws (20 available)
  expect_error(
    subset_draws_tail(draws_array, posterior_size = 30),
    err_pattern(ERR_POSTERIOR_SIZE_EXCEEDS, posterior_size = 30, n_draws = 20)
  )
})
