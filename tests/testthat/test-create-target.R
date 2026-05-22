
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
  return(fit)
}

test_that("internal_target_builder_vec works with mode='error'", {
  # Successful case
  res <- internal_target_builder_vec(
    location = c("schlA", "schlB"),
    age = c(2L, 3L),
    cohort = c(5L, 6L),
    dose = c(1L, 2L),
    mode = "error"
  )

  expect_s3_class(res, "data.table")
  expect_equal(names(res), c("obs_c_id", "loc_id", "age", "cohort", "dose", "weight"))
  expect_equal(res$obs_c_id, 1:2)
  expect_equal(res$loc_id, c("schlA", "schlB"))
  expect_equal(res$weight, c(1, 1))

  # Length mismatch case
  expect_error(
    internal_target_builder_vec(
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
    internal_target_builder_vec(
      location = character(0),
      age = c(2L),
      cohort = c(5L),
      dose = c(1L),
      mode = "error"
    ),
    "No arguments may have length zero; the following do: location"
  )

  expect_error(
    internal_target_builder_vec(
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
    internal_target_builder_vec(
      location = c("schlA", "schlB"),
      mode = "error"
    ),
    "age, cohort, and dose must be supplied"
  )
})

test_that("internal_target_builder_vec works with mode='enumerate'", {
  res <- internal_target_builder_vec(
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
})

test_that("internal_target_builder_vec works with mode='recycle'", {
  res <- internal_target_builder_vec(
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

test_that("internal_target_builder_df works and creates target successfully", {
  df_loc <- data.frame(
    loc_id = c("schlA", "schlB"),
    age = c(1L, 2L),
    cohort = c(3L, 4L),
    dose = c(1L, 2L)
  )

  # Successful case
  res <- internal_target_builder_df(df_loc)
  expect_s3_class(res, "data.table")
  expect_equal(names(res), c("obs_c_id", "loc_id", "age", "cohort", "dose", "weight"))
  expect_equal(res$obs_c_id, 1:2)
  expect_equal(res$weight, c(1, 1))

  # Error when required columns are missing
  bad_df <- df_loc[, c("loc_id", "age")]
  expect_error(
    internal_target_builder_df(bad_df),
    "missing the following required column"
  )
})

test_that("internal_target_builder_df validates custom obs_c_id and weight columns", {
  df_custom <- data.frame(
    loc_id = c("schlA", "schlB"),
    age = c(1L, 2L),
    cohort = c(3L, 4L),
    dose = c(1L, 2L),
    obs_c_id = 1:2,
    weight = c(1, 1)
  )
  res <- internal_target_builder_df(df_custom)
  expect_equal(res$obs_c_id, 1:2)
  expect_equal(res$weight, c(1, 1))

  # Invalid obs_c_id
  df_bad_id <- df_custom
  df_bad_id$obs_c_id <- c(1, 3)
  expect_error(
    internal_target_builder_df(df_bad_id),
    "if supplied, obs_c_id must be 1:nrow"
  )

  # Invalid weight
  df_bad_wt <- df_custom
  df_bad_wt$weight <- c(1, -2)
  expect_error(
    internal_target_builder_df(df_bad_wt),
    "if supplied, weight must be"
  )
})

test_that("create_target correctly delegates to vector/df builders and processes location mappings", {
  fit <- make_mock_fit()

  # Test vector branch delegation
  res_vec <- create_target(
    fit = fit,
    location = c("schlA", "schlB"),
    age = c(2L, 3L),
    cohort = c(5L, 6L),
    dose = c(1L, 2L),
    mode = "error"
  )
  expect_s3_class(res_vec, "data.table")
  expect_equal(names(res_vec), c("obs_c_id", "loc_id", "age", "cohort", "dose", "weight", "loc_c_id"))
  expect_equal(res_vec$loc_c_id, c(4, 5))

  # Test df branch delegation
  df_loc <- data.frame(
    loc_id = c("schlA", "schlB"),
    age = c(1L, 2L),
    cohort = c(3L, 4L),
    dose = c(1L, 2L)
  )
  res_df <- create_target(fit = fit, location = df_loc)
  expect_s3_class(res_df, "data.table")
  expect_equal(names(res_df), c("obs_c_id", "loc_id", "age", "cohort", "dose", "weight", "loc_c_id"))
  expect_equal(res_df$loc_c_id, c(4, 5))

  # Error when additional arguments are supplied for df
  expect_error(
    create_target(fit = fit, location = df_loc, age = 1L),
    "age, cohort, and dose must not be supplied"
  )
})

test_that("create_target performs correct bounds checking against fit object", {
  fit <- make_mock_fit()

  # Location bounds mismatch
  expect_error(
    create_target(
      fit = fit,
      location = "unknown_loc",
      age = 1L,
      cohort = 1L,
      dose = 1L
    ),
    "all locations must be within fit\\$locations. Invalid locations: unknown_loc"
  )

  # Dose bounds mismatch
  expect_error(
    create_target(
      fit = fit,
      location = "schlA",
      age = 1L,
      cohort = 1L,
      dose = 3L
    ),
    "dose values must be within 1 and fit\\$data\\$n_doses \\(2\\)\\. Invalid dose in rows: 1"
  )

  # Age bounds mismatch
  expect_error(
    create_target(
      fit = fit,
      location = "schlA",
      age = 6L,
      cohort = 1L,
      dose = 1L
    ),
    "age values must be within 1 and fit\\$data\\$n_yr \\(5\\)\\. Invalid age in rows: 1"
  )

  # Cohort bounds mismatch
  expect_error(
    create_target(
      fit = fit,
      location = "schlA",
      age = 1L,
      cohort = 11L,
      dose = 1L
    ),
    "cohort values must be within 1 and fit\\$data\\$n_cohort \\(10\\)\\. Invalid cohort in rows: 1"
  )
})
