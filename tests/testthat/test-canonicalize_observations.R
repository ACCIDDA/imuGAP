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

  err_pos <- tryCatch(
    canonicalize_observations(obs_pos_samp_inconsistent),
    error = identity
  )
  expect_match(
    err_pos$message,
    err_pattern(ERR_OBS_POS_GT_SAMPLE, n_rows = 1L, obs_ids = "b")
  )
  diag_pos <- eval_err_diagnostic(
    err_pos,
    list(observations = obs_pos_samp_inconsistent)
  )
  expect_equal(diag_pos$obs_id, "b")
})

test_that("errors when obs_id contains NA", {
  obs <- data.frame(
    obs_id = c("a", NA, "c"),
    positive = c(5, 10, 15),
    sample_n = c(10, 20, 30)
  )
  err_na <- tryCatch(canonicalize_observations(obs), error = identity)
  expect_match(
    err_na$message,
    err_pattern(ERR_OBS_NA_ID, n_nas = 1L, rows = "2")
  )
  diag_na <- eval_err_diagnostic(err_na, list(observations = obs))
  expect_true(is.na(diag_na$obs_id))
})

test_that("errors when obs_id has duplicates", {
  obs <- data.frame(
    obs_id = c("a", "b", "a"),
    positive = c(5, 10, 15),
    sample_n = c(10, 20, 30)
  )
  err_dup <- tryCatch(canonicalize_observations(obs), error = identity)
  expect_match(
    err_dup$message,
    err_pattern(ERR_OBS_DUP_ID, n_duplicates = 1L, duplicates = "3")
  )
  diag_dup <- eval_err_diagnostic(err_dup, list(observations = obs))
  expect_equal(diag_dup$obs_id, c("a", "a"))
})

test_that("errors when censored column is not numeric", {
  obs <- data.frame(
    obs_id = c("a", "b", "c"),
    positive = c(5, 10, 15),
    sample_n = c(10, 20, 30),
    censored = c("no", "yes", "no")
  )
  expect_error(
    canonicalize_observations(obs),
    err_pattern(ERR_OBS_CENSORED_NUMERIC)
  )
})

test_that("errors when censored column contains values other than NA or 1", {
  obs <- data.frame(
    obs_id = c("a", "b", "c"),
    positive = c(5, 10, 15),
    sample_n = c(10, 20, 30),
    censored = c(NA, 0, 1) # 0 is not allowed (reserved for left-censoring)
  )
  err_cens <- tryCatch(canonicalize_observations(obs), error = identity)
  expect_match(err_cens$message, err_pattern(ERR_OBS_CENSORED_VALUES))
  diag_cens <- eval_err_diagnostic(err_cens, list(observations = obs))
  expect_equal(diag_cens$obs_id, "b")
  expect_equal(diag_cens$censored, 0)
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

# --- structured errors (#156) -------------------------------------------------

test_that("positive > sample_n reports the offending rows", {
  obs <- make_test_obs()
  obs$positive[2] <- obs$sample_n[2] + 1L
  err <- tryCatch(canonicalize_observations(obs), error = identity)
  expect_identical(err$id, "ERR_OBS_POS_GT_SAMPLE")
  expect_identical(err$rows, which(obs$positive > obs$sample_n))
})

test_that("invalid censored values report the offending rows", {
  obs <- make_test_obs()
  obs$censored <- c(NA, 2)
  err <- tryCatch(canonicalize_observations(obs), error = identity)
  expect_identical(err$id, "ERR_OBS_CENSORED_VALUES")
  expect_identical(err$rows, 2L)
})

test_that("an NA positive names `observations` and reports the row", {
  obs <- make_test_obs()
  obs$positive[2] <- NA
  err <- tryCatch(canonicalize_observations(obs), error = identity)
  expect_identical(err$name, "observations")
  expect_identical(err$col, "positive")
  expect_identical(err$rows, 2L)
})

test_that("an all-NA logical censored column means none censored", {
  obs <- make_test_obs()
  # what read.csv() gives for an empty column
  obs$censored <- NA
  expect_type(obs$censored, "logical")
  expect_silent(res <- canonicalize_observations(obs))
  expect_type(res$censored, "double")
  expect_true(all(is.na(res$censored)))
})
