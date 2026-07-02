# Setup mock fit object for create_target validation
make_mock_fit <- function() {
  fit <- list(
    locations = data.frame(
      loc_id = c("state", "cnty1", "cnty2", "schlA", "schlB"),
      loc_c_id = 1:5
    ),
    data = list(
      n_doses = 2L,
      n_yr = 5L,
      n_cohort = 10L
    )
  )
  class(fit) <- "imugap_fit"
  fit
}

test_that("create_target builds a grid with mode='error'", {
  # Successful case
  res <- create_target(
    location = c("schlA", "schlB"),
    age = c(2L, 3L),
    cohort = c(5L, 6L),
    dose = c(1L, 2L),
    mode = "error"
  )

  expect_s3_class(res, "data.table")
  expect_equal(
    names(res),
    c("obs_c_id", "loc_id", "age", "cohort", "dose", "weight")
  )
  expect_equal(res$obs_c_id, 1:2)
  expect_equal(res$loc_id, c("schlA", "schlB"))
  expect_equal(res$weight, c(1, 1))

  # Length mismatch case
  expect_error(
    create_target(
      location = c("schlA", "schlB"),
      age = c(2L),
      cohort = c(5L, 6L),
      dose = c(1L, 2L),
      mode = "error"
    ),
    "All arguments must have the same length"
  )

  # Zero length cases
  expect_error(
    create_target(
      location = character(0),
      age = c(2L),
      cohort = c(5L),
      dose = c(1L),
      mode = "error"
    ),
    "No arguments may have length zero; the following do: location"
  )

  expect_error(
    create_target(
      location = character(0),
      age = integer(0),
      cohort = c(5L),
      dose = c(1L),
      mode = "error"
    ),
    "No arguments may have length zero; the following do: location, age"
  )

  # Missing arguments case
  expect_error(
    create_target(
      location = c("schlA", "schlB"),
      mode = "error"
    ),
    "age, cohort, and dose must be supplied"
  )

  # NA cases
  expect_error(
    create_target(
      location = c("schlA", NA),
      age = c(2L),
      cohort = c(5L),
      dose = c(1L),
      mode = "error"
    ),
    "No arguments may have NA values; the following do: location"
  )

  expect_error(
    create_target(
      location = c("schlA"),
      age = c(NA_integer_),
      cohort = c(NA_integer_),
      dose = c(1L),
      mode = "error"
    ),
    "No arguments may have NA values; the following do: age, cohort"
  )
})

test_that("create_target builds a grid with mode='enumerate'", {
  res <- create_target(
    location = c("schlA", "schlB"),
    age = c(2L, 3L),
    cohort = c(5L),
    dose = c(1L, 2L),
    mode = "enumerate"
  )

  # Total combinations = 2 (locations) * 2 (ages) * 1 (cohort) * 2 (doses) = 8
  expect_equal(nrow(res), 8)
  expect_equal(res$obs_c_id, 1:8)
  expect_true(all(res$weight == 1.0))
  expect_setequal(res$loc_id, c("schlA", "schlB"))

  # Test with all arguments having length > 1
  res_multi <- create_target(
    location = c("schlA", "schlB"),
    age = c(2L, 3L),
    cohort = c(5L, 6L),
    dose = c(1L, 2L),
    mode = "enumerate"
  )

  # Total combinations = 2 * 2 * 2 * 2 = 16
  expect_equal(nrow(res_multi), 16)
  expect_equal(res_multi$obs_c_id, 1:16)
  expect_true(all(res_multi$weight == 1.0))
  expect_setequal(res_multi$cohort, c(5L, 6L))
})

test_that("create_target builds a grid with mode='recycle'", {
  res <- create_target(
    location = c("schlA", "schlB"),
    age = c(2L, 3L, 4L),
    cohort = c(5L),
    dose = c(1L, 2L),
    mode = "recycle"
  )

  # LCM of 2, 3, 1, 2 is 6
  expect_equal(nrow(res), 6)
  expect_equal(res$obs_c_id, 1:6)
  expect_equal(res$loc_id, rep(c("schlA", "schlB"), 3))
  expect_equal(res$age, rep(c(2L, 3L, 4L), 2))
  expect_equal(res$dose, rep(c(1L, 2L), 3))
})

test_that("canonicalize_target normalizes a plain data.frame target", {
  fit <- make_mock_fit()
  df_loc <- data.frame(
    loc_id = c("schlA", "schlB"),
    age = c(1L, 2L),
    cohort = c(3L, 4L),
    dose = c(1L, 2L)
  )

  # Successful case: fills obs_c_id/weight, validates, and adds loc_c_id
  res <- canonicalize_target(df_loc, fit)
  expect_s3_class(res, "data.table")
  expect_equal(
    names(res),
    c("obs_c_id", "loc_id", "age", "cohort", "dose", "weight", "loc_c_id")
  )
  expect_equal(res$obs_c_id, 1:2)
  expect_equal(res$weight, c(1, 1))

  # Error when required columns are missing
  bad_df <- df_loc[, c("loc_id", "age")]
  expect_error(
    canonicalize_target(bad_df, fit),
    "missing the following required column"
  )
})

test_that("canonicalize_target validates custom obs_c_id, weight, and obs_id columns", {
  fit <- make_mock_fit()
  df_custom <- data.frame(
    loc_id = c("schlA", "schlB"),
    age = c(1L, 2L),
    cohort = c(3L, 4L),
    dose = c(1L, 2L),
    obs_c_id = 1:2,
    weight = c(1, 1)
  )
  res <- canonicalize_target(df_custom, fit)
  expect_equal(res$obs_c_id, 1:2)
  expect_equal(res$weight, c(1, 1))

  # Invalid obs_c_id
  df_bad_id <- df_custom
  df_bad_id$obs_c_id <- c(1, 3)
  expect_error(
    canonicalize_target(df_bad_id, fit),
    "if supplied, obs_c_id must be 1:nrow"
  )

  # Invalid weight
  df_bad_wt <- df_custom
  df_bad_wt$weight <- c(1, -2)
  expect_error(
    canonicalize_target(df_bad_wt, fit),
    "if supplied, weight must be"
  )

  # Non-unique obs_id with a weight column: not yet supported (see #79).
  df_dup_obs_weighted <- df_custom
  df_dup_obs_weighted$obs_id <- c("o1", "o1")
  expect_error(
    canonicalize_target(df_dup_obs_weighted, fit),
    "not yet supported"
  )

  # Non-unique obs_id without weights still hits the plain uniqueness check.
  df_dup_obs <- df_custom
  df_dup_obs$obs_id <- c("o1", "o1")
  df_dup_obs$weight <- NULL
  expect_error(
    canonicalize_target(df_dup_obs, fit),
    "if supplied, obs_id must be unique"
  )
})

test_that("canonicalize_target maps loc_c_id for create_target output and data.frame targets", {
  fit <- make_mock_fit()

  # Test vector branch delegation
  res_vec <- canonicalize_target(create_target(
    location = c("schlA", "schlB"),
    age = c(2L, 3L),
    cohort = c(5L, 6L),
    dose = c(1L, 2L),
    mode = "error"
  ), fit)
  expect_s3_class(res_vec, "data.table")
  expect_equal(
    names(res_vec),
    c("obs_c_id", "loc_id", "age", "cohort", "dose", "weight", "loc_c_id")
  )
  expect_equal(res_vec$loc_c_id, c(4, 5))

  # canonicalize_target accepts a plain data.frame directly
  df_loc <- data.frame(
    loc_id = c("schlA", "schlB"),
    age = c(1L, 2L),
    cohort = c(3L, 4L),
    dose = c(1L, 2L)
  )
  res_df <- canonicalize_target(df_loc, fit)
  expect_s3_class(res_df, "data.table")
  expect_equal(
    names(res_df),
    c("obs_c_id", "loc_id", "age", "cohort", "dose", "weight", "loc_c_id")
  )
  expect_equal(res_df$loc_c_id, c(4, 5))
})

test_that("canonicalize_target performs correct bounds checking against fit object", {
  fit <- make_mock_fit()

  # Location bounds mismatch
  expect_error(
    canonicalize_target(create_target(
      location = "unknown_loc",
      age = 1L,
      cohort = 1L,
      dose = 1L
    ), fit),
    "all locations must be within fit\\$locations. Invalid locations: unknown_loc"
  )

  # Dose bounds mismatch
  expect_error(
    canonicalize_target(create_target(
      location = "schlA",
      age = 1L,
      cohort = 1L,
      dose = 3L
    ), fit),
    "dose values must be within 1 and fit\\$data\\$n_doses \\(2\\)\\. Invalid dose in rows: 1"
  )

  # Age bounds mismatch
  expect_error(
    canonicalize_target(create_target(
      location = "schlA",
      age = 6L,
      cohort = 1L,
      dose = 1L
    ), fit),
    "age values must be within 1 and fit\\$data\\$n_yr \\(5\\)\\. Invalid age in rows: 1"
  )

  # Cohort bounds mismatch
  expect_error(
    canonicalize_target(create_target(
      location = "schlA",
      age = 1L,
      cohort = 11L,
      dose = 1L
    ), fit),
    "cohort values must be within 1 and fit\\$data\\$n_cohort \\(10\\)\\. Invalid cohort in rows: 1"
  )
})

test_that("create_target builds a grid with mode='snapshot'", {
  # Successful case
  res <- create_target(
    location = c("schlA", "schlB"),
    age = c(2L, 3L, 4L),
    cohort = c(5L),
    dose = c(1L, 2L),
    mode = "snapshot"
  )

  expect_s3_class(res, "data.table")
  expect_equal(
    names(res),
    c("obs_c_id", "loc_id", "age", "cohort", "dose", "weight")
  )
  expect_equal(nrow(res), 12)
  expect_equal(res$obs_c_id, 1:12)
  expect_true(all(res$weight == 1.0))

  # Constant sum check: max_age = 4, ref_cohort = 5, sum = 9
  expect_true(all(res$age + res$cohort == 9L))

  # Specific mappings
  expect_equal(unique(res[age == 4L, cohort]), 5L)
  expect_equal(unique(res[age == 3L, cohort]), 6L)
  expect_equal(unique(res[age == 2L, cohort]), 7L)

  # Error when cohort is not a single reference value
  expect_error(
    create_target(
      location = c("schlA", "schlB"),
      age = c(2L, 3L),
      cohort = c(5L, 6L),
      dose = c(1L, 2L),
      mode = "snapshot"
    ),
    "cohort must be a single reference value in 'snapshot' mode"
  )
})

test_that("create_target works with mode='snapshot' and validates output", {
  fit <- make_mock_fit()

  # Successful case
  res <- canonicalize_target(create_target(
    location = c("schlA", "schlB"),
    age = c(2L, 3L, 4L),
    cohort = 5L,
    dose = c(1L, 2L),
    mode = "snapshot"
  ), fit)
  expect_s3_class(res, "data.table")
  expect_equal(
    names(res),
    c("obs_c_id", "loc_id", "age", "cohort", "dose", "weight", "loc_c_id")
  )
  expect_true(all(res$age + res$cohort == 9L))
  expect_equal(res$loc_c_id, rep(c(4, 5), times = 6))

  # Cohort bounds mismatch via calculated cohort (constant sum exceeds bounds)
  expect_error(
    canonicalize_target(create_target(
      location = "schlA",
      age = c(1L, 2L, 4L),
      cohort = 8L,
      dose = 1L,
      mode = "snapshot"
    ), fit),
    "cohort values must be within 1 and fit\\$data\\$n_cohort \\(10\\)\\. Invalid cohort in rows: 1"
  )
})
