# Tests for imugap_options()

test_that("imugap_options returns expected structure with defaults", {
  defaults <- imugap_options()
  expect_type(defaults, "list")
  expect_setequal(
    names(defaults),
    c(
      "df",
      "dose_schedule",
      "compute_log_lik",
      "link",
      "model",
      "guess",
      "guess_type",
      "solver",
      "solver_type"
    )
  )
  expect_equal(defaults$df, 5L)
  expect_equal(defaults$dose_schedule, c(1, 4))
  expect_false(defaults$compute_log_lik)
  expect_equal(defaults$link, "default")
  expect_equal(defaults$model, "default")
  expect_equal(defaults$guess, "taylor4")
  expect_equal(defaults$guess_type, 3L)
  expect_equal(defaults$solver, "direct")
  expect_equal(defaults$solver_type, 1L)
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

test_that("imugap_options errors on unknown link / model", {
  expect_error(
    imugap_options(link = "unknown_link"),
    "should be"
  )
  expect_error(
    imugap_options(model = "unknown_model"),
    "should be"
  )
  expect_error(
    imugap_options(link = "stateonly"),
    "should be"
  )
})

test_that("imugap_options accepts default, logit, and probit link options", {
  defaults <- imugap_options()
  opts_default <- imugap_options(link = "default")
  expect_equal(opts_default, defaults)

  opts_logit <- imugap_options(link = "logit")
  expect_equal(opts_logit$link, "logit")
  expect_equal(opts_logit$model, "logit")

  opts_probit <- imugap_options(link = "probit")
  expect_equal(opts_probit$link, "probit")
  expect_equal(opts_probit$model, "probit")

  # Backwards compatibility with model parameter
  opts_model_probit <- imugap_options(model = "probit")
  expect_equal(opts_model_probit$link, "probit")
  expect_equal(opts_model_probit$model, "probit")
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

test_that("imugap_options rejects invalid compute_log_lik", {
  expect_error(
    imugap_options(compute_log_lik = "TRUE"),
    err_pattern(ERR_OPT_COMPUTE_LOG_LIK)
  )
  expect_error(
    imugap_options(compute_log_lik = c(TRUE, FALSE)),
    err_pattern(ERR_OPT_COMPUTE_LOG_LIK)
  )
  expect_error(
    imugap_options(compute_log_lik = NA),
    err_pattern(ERR_OPT_COMPUTE_LOG_LIK)
  )
})

test_that("imugap_options accepts valid guess and solver values including default", {
  opts_def <- imugap_options(guess = "default", solver = "default")
  expect_equal(opts_def$guess, "taylor4")
  expect_equal(opts_def$guess_type, 3L)
  expect_equal(opts_def$solver, "direct")
  expect_equal(opts_def$solver_type, 1L)

  opts <- imugap_options(guess = "zero", solver = "builtin")
  expect_equal(opts$guess, "zero")
  expect_equal(opts$guess_type, 1L)
  expect_equal(opts$solver, "builtin")
  expect_equal(opts$solver_type, 4L)

  opts_num <- imugap_options(guess = 2L, solver = 3L)
  expect_equal(opts_num$guess, "taylor2")
  expect_equal(opts_num$guess_type, 2L)
  expect_equal(opts_num$solver, "newton2")
  expect_equal(opts_num$solver_type, 3L)
})

test_that("imugap_options rejects invalid guess and solver values", {
  expect_error(imugap_options(guess = "invalid_guess"), "should be")
  expect_error(imugap_options(solver = "invalid_solver"), "should be")
})
