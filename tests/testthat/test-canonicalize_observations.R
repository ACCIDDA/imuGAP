test_that("works for obvious case", {
  # Test without id column
  obs <- data.frame(
    id = c("a", "b", "c"),
    positive = c(5, 10, 15),
    sample_n = c(10, 20, 30)
  )

  expect_silent(obs_res <- canonicalize_observations(obs))
  expect_s3_class(obs_res, "data.table")
  expect_equal(obs_res$obs_id, 1:3)

})

test_that("works with censoring", {
  # Test without id column
  obs <- data.frame(
    id = c("a", "b", "c"),
    positive = c(5, 10, 15),
    sample_n = c(10, 20, 30),
    censored = c(NA, 1, NA)
  )

  expect_silent(obs_res <- canonicalize_observations(obs))
  expect_s3_class(obs_res, "data.table")
  expect_equal(obs_res$id, c("a", "c", "b"))

})

test_that("can ensure scientific validity", {

  obs_negative_pos <- data.frame(
    id = c("a", "b", "c"),
    positive = c(5, -10, 15),
    sample_n = c(10, 20, 30)
  )

  expect_error(canonicalize_observations(obs_negative_pos), "positive")

  obs_negative_n <- data.frame(
    id = c("a", "b", "c"),
    positive = c(5, 10, 15),
    sample_n = c(10, -20, 30)
  )

  expect_error(canonicalize_observations(obs_negative_n), "sample_n")

  obs_pos_samp_inconsistent <- data.frame(
    id = c("a", "b", "c"),
    positive = c(5, 10, 15),
    sample_n = c(10, 9, 30)
  )

  expect_error(canonicalize_observations(obs_pos_samp_inconsistent), "sample_n")

})