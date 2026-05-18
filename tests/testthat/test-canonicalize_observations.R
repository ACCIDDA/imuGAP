test_that("works for obvious case", {
  # Test without id column
  obs <- data.frame(
    obs_id = c("a", "b", "c"),
    positive = c(5, 10, 15),
    sample_n = c(10, 20, 30)
  )

  expect_silent(obs_res <- canonicalize_observations(obs))
  expect_s3_class(obs_res, "data.table")
  expect_equal(obs_res$obs_c_id, 1:3)
})

test_that("keeps or discards extra cols", {
  # Test without id column
  obs <- data.frame(
    obs_id = c("a", "b", "c"),
    positive = c(5, 10, 15),
    sample_n = c(10, 20, 30),
    extra = rep(TRUE, 3)
  )

  expect_silent(obs_res <- canonicalize_observations(obs))
  expect_false("extra" %in% names(obs_res))

  expect_silent(obs_res2 <- canonicalize_observations(obs, drop_extra = FALSE))
  expect_true("extra" %in% names(obs_res2))
})

test_that("works with censoring", {
  obs <- data.frame(
    obs_id = c("a", "b", "c"),
    positive = c(5, 10, 15),
    sample_n = c(10, 20, 30),
    censored = c(NA, 1, NA)
  )

  expect_silent(obs_res <- canonicalize_observations(obs))
  expect_s3_class(obs_res, "data.table")
  expect_equal(obs_res$obs_id, c("a", "c", "b"))
})

test_that("can ensure scientific validity", {
  obs_negative_pos <- data.frame(
    obs_id = c("a", "b", "c"),
    positive = c(5, -10, 15),
    sample_n = c(10, 20, 30)
  )

  expect_error(canonicalize_observations(obs_negative_pos), "positive")

  obs_negative_n <- data.frame(
    obs_id = c("a", "b", "c"),
    positive = c(5, 10, 15),
    sample_n = c(10, -20, 30)
  )

  expect_error(canonicalize_observations(obs_negative_n), "sample_n")

  obs_pos_samp_inconsistent <- data.frame(
    obs_id = c("a", "b", "c"),
    positive = c(5, 10, 15),
    sample_n = c(10, 9, 30)
  )

  expect_error(canonicalize_observations(obs_pos_samp_inconsistent), "sample_n")
})

test_that("errors when obs_id contains NA", {
  obs <- data.frame(
    obs_id = c("a", NA, "c"),
    positive = c(5, 10, 15),
    sample_n = c(10, 20, 30)
  )
  expect_error(canonicalize_observations(obs), "obs_id.*NA")
})

test_that("errors when obs_id has duplicates", {
  obs <- data.frame(
    obs_id = c("a", "b", "a"),
    positive = c(5, 10, 15),
    sample_n = c(10, 20, 30)
  )
  expect_error(canonicalize_observations(obs), "unique")
})

test_that("errors when censored column is not numeric", {
  obs <- data.frame(
    obs_id = c("a", "b", "c"),
    positive = c(5, 10, 15),
    sample_n = c(10, 20, 30),
    censored = c("no", "yes", "no")
  )
  expect_error(canonicalize_observations(obs), "censored.*numeric")
})

test_that("errors when censored column contains values other than NA or 1", {
  obs <- data.frame(
    obs_id = c("a", "b", "c"),
    positive = c(5, 10, 15),
    sample_n = c(10, 20, 30),
    censored = c(NA, 0, 1) # 0 is not allowed (reserved for left-censoring)
  )
  expect_error(canonicalize_observations(obs), "censored")
})

test_that("canonical input short-circuits and returns unchanged", {
  obs <- data.frame(
    obs_id = c("a", "b", "c"),
    positive = c(5, 10, 15),
    sample_n = c(10, 20, 30)
  )
  canon <- canonicalize_observations(obs)
  again <- canonicalize_observations(canon)
  expect_identical(canon, again)
})
