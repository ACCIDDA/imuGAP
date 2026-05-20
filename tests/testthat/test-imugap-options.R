# Tests for imugap_options()

test_that("imugap_options returns expected structure with defaults", {
  opts <- imugap_options()
  expect_type(opts, "list")
  expect_setequal(names(opts), c("df", "dose_schedule", "object"))
  expect_equal(opts$df, 5L)
  expect_equal(opts$dose_schedule, c(1, 4))
  expect_s4_class(opts$object, "stanmodel")
})

test_that("imugap_options default object is the v6 stanmodel", {
  opts <- imugap_options()
  expect_equal(opts$object@model_name, "impute_school_coverage_process_v6")
})

test_that("imugap_options df can be overridden", {
  opts <- imugap_options(df = 10L)
  expect_equal(opts$df, 10L)
  expect_equal(opts$dose_schedule, c(1, 4))
  expect_s4_class(opts$object, "stanmodel")
})

test_that("imugap_options dose_schedule can be overridden", {
  opts <- imugap_options(dose_schedule = c(2, 5, 7))
  expect_equal(opts$dose_schedule, c(2, 5, 7))
  expect_equal(opts$df, 5L)
})

test_that("imugap_options errors on unknown object", {
  expect_error(
    imugap_options(object = "unknown_model"),
    "Unknown model object"
  )
  expect_error(
    imugap_options(object = "stateonly"),
    "Unknown model object"
  )
})

test_that("imugap_options accepts default keyword explicitly", {
  opts <- imugap_options(object = "default")
  expect_s4_class(opts$object, "stanmodel")
  expect_equal(opts$object@model_name, "impute_school_coverage_process_v6")
})

test_that("imugap_options accepts numeric whole-number df", {
  opts <- imugap_options(df = 4)
  expect_equal(opts$df, 4L)
})

test_that("imugap_options rejects invalid df", {
  expect_error(imugap_options(df = -5L), "df")
  expect_error(imugap_options(df = 0L), "df")
  expect_error(imugap_options(df = 5.5), "df")
  expect_error(imugap_options(df = c(5L, 5L)), "df")
  expect_error(imugap_options(df = NA_integer_), "df")
  expect_error(imugap_options(df = "5"), "df")
})

test_that("imugap_options rejects invalid dose_schedule", {
  valid_schedule <- c(1, 4)
  expect_error(
    imugap_options(dose_schedule = -valid_schedule),
    "dose_schedule"
  )
  expect_error(
    imugap_options(dose_schedule = c(0, valid_schedule)),
    "dose_schedule"
  )
  expect_error(
    imugap_options(dose_schedule = c(valid_schedule, NA)),
    "dose_schedule"
  )
  expect_error(
    imugap_options(dose_schedule = numeric(0)),
    "dose_schedule"
  )
  expect_error(
    imugap_options(dose_schedule = as.character(valid_schedule)),
    "dose_schedule"
  )
  expect_error(
    imugap_options(dose_schedule = rev(valid_schedule)),
    "dose_schedule"
  )
  expect_error(
    imugap_options(dose_schedule = c(1.5, 4)),
    "dose_schedule"
  )
})

test_that("imugap_options coerces dose_schedule to integer", {
  opts <- imugap_options(dose_schedule = c(2, 5, 7))
  expect_type(opts$dose_schedule, "integer")
  expect_equal(opts$dose_schedule, c(2L, 5L, 7L))
})
