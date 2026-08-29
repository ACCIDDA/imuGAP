# Tests for imugap_options()

test_that("imugap_options returns expected structure with defaults", {
  defaults <- imugap_options()
  expect_type(defaults, "list")
  expect_setequal(names(defaults), c("df", "dose_schedule", "model"))
  expect_equal(defaults$df, 5L)
  expect_equal(defaults$dose_schedule, c(1, 4))
  expect_equal(defaults$model, "default")
})

test_that("imugap_options df can be overridden", {
  defaults <- imugap_options()
  override_df <- 10L
  opts <- imugap_options(df = override_df)
  expected <- defaults
  expected$df <- override_df
  expect_equal(opts, expected)
})

test_that("imugap_options dose_schedule can be overridden", {
  defaults <- imugap_options()
  override_sched <- c(2, 5, 7)
  opts <- imugap_options(dose_schedule = override_sched)
  expected <- defaults
  expected$dose_schedule <- override_sched
  expect_equal(opts, expected)
})

test_that("imugap_options errors on unknown model", {
  expect_error(
    imugap_options(model = "unknown_model"),
    "should be"
  )
  expect_error(
    imugap_options(model = "stateonly"),
    "should be"
  )
})

test_that("imugap_options accepts default keyword explicitly", {
  defaults <- imugap_options()
  opts <- imugap_options(model = "default")
  expect_equal(opts, defaults)
})

test_that("imugap_options accepts numeric whole-number df", {
  defaults <- imugap_options()
  override_df <- 4
  opts <- imugap_options(df = override_df)
  expected <- defaults
  expected$df <- as.integer(override_df)
  expect_equal(opts, expected)
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
  defaults <- imugap_options()
  override_sched <- c(2, 5, 7)
  opts <- imugap_options(dose_schedule = override_sched)
  expected <- defaults
  expected$dose_schedule <- as.integer(override_sched)
  expect_type(opts$dose_schedule, "integer")
  expect_equal(opts, expected)
})
