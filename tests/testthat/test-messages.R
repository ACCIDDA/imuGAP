test_that("format_message works with named placeholders", {
  tpl <- "dose {dose} requires age > {sched_age} (`dose_schedule[{dose}] == {sched_age}`)"
  res <- format_message(tpl, dose = 2L, sched_age = 4L)
  expect_identical(
    res,
    "dose 2 requires age > 4 (`dose_schedule[2] == 4`)"
  )
})

test_that("format_message collapses vector arguments via toString", {
  tpl <- "missing column(s): {missing}"
  res <- format_message(tpl, missing = c("age", "cohort", "dose"))
  expect_identical(res, "missing column(s): age, cohort, dose")

  tpl_nums <- "duplicate ID(s): {dupes}"
  res_nums <- format_message(tpl_nums, dupes = c(1L, 4L, 9L))
  expect_identical(res_nums, "duplicate ID(s): 1, 4, 9")
})

test_that("format_message works with sprintf positional placeholders", {
  res <- format_message("found %d errors in %s", 3L, "column 'a'")
  expect_identical(res, "found 3 errors in column 'a'")
})

test_that("format_message returns template untouched when no args provided", {
  tpl <- "static message with no params"
  expect_identical(format_message(tpl), tpl)
})

test_that("stop_fmt_if works with named placeholders and call attribution", {
  test_fn <- function(k, sched) {
    stop_fmt_if(
      TRUE,
      "dose {dose} requires age > {sched_age}",
      dose = k,
      sched_age = sched
    )
  }

  err <- tryCatch(test_fn(2L, 4L), error = identity)
  expect_s3_class(err, "error")
  expect_identical(err$message, "dose 2 requires age > 4")
  expect_equal(deparse(err$call), "test_fn(2L, 4L)")
})

test_that("stop_fmt_if formats vector arguments without vectorizing error object", {
  test_fn <- function(bad_cols) {
    stop_fmt_if(
      TRUE,
      "invalid column(s): {cols}",
      cols = bad_cols
    )
  }

  err <- tryCatch(test_fn(c("x", "y")), error = identity)
  expect_s3_class(err, "error")
  expect_identical(err$message, "invalid column(s): x, y")
  expect_length(err$message, 1L)
})

test_that("warn_fmt_if works with named placeholders and returns boolean", {
  test_fn <- function(warn, k) {
    warn_fmt_if(warn, "warning for item {item}", item = k)
  }

  expect_false(expect_silent(test_fn(FALSE, 1L)))

  warn_obj <- tryCatch(test_fn(TRUE, 5L), warning = identity)
  expect_s3_class(warn_obj, "warning")
  expect_identical(warn_obj$message, "warning for item 5")
})

test_that("eval_err_diagnostic extracts and executes subset diagnostic correctly", {
  df <- data.frame(
    obs_id = c("a", "b", "c"),
    positive = c(5, 25, 15),
    sample_n = c(10, 20, 30)
  )
  err <- tryCatch(
    stop_fmt_if(
      TRUE,
      ERR_OBS_POS_GT_SAMPLE,
      n_rows = 1L,
      obs_ids = "b"
    ),
    error = identity
  )

  bad_rows <- eval_err_diagnostic(err, list(observations = df))
  expect_equal(bad_rows$obs_id, "b")
  expect_equal(bad_rows$positive, 25)
})

# --- structured conditions (#156) ---------------------------------------------

test_that("stop_fmt_if raises an imugap_error carrying the template id and fields", {
  test_fn <- function(bad_rows) {
    stop_fmt_if(
      TRUE,
      ERR_CANNOT_HAVE_NA,
      name = "observations",
      col = "positive",
      rows = bad_rows
    )
  }
  err <- tryCatch(test_fn(c(2L, 7L)), error = identity)

  expect_s3_class(err, c("imugap_error", "error", "condition"), exact = TRUE)
  expect_identical(err$id, "ERR_CANNOT_HAVE_NA")
  # named arguments become fields, with their raw values (not toString()'d)
  expect_identical(err$name, "observations")
  expect_identical(err$col, "positive")
  expect_identical(err$rows, c(2L, 7L))
  # fields absent from the template are carried but not printed
  expect_match(
    conditionMessage(err),
    err_pattern(ERR_CANNOT_HAVE_NA, name = "observations")
  )
  expect_no_match(conditionMessage(err), "7")
  expect_equal(deparse(err$call), "test_fn(c(2L, 7L))")
})

test_that("an imugap_error is caught by class and by plain error handlers", {
  signal <- function() stop_fmt_if(TRUE, ERR_OBS_CENSORED_NUMERIC)
  expect_identical(
    tryCatch(signal(), imugap_error = function(e) e$id),
    "ERR_OBS_CENSORED_NUMERIC"
  )
  expect_error(signal(), class = "imugap_error")
  expect_error(signal(), err_pattern(ERR_OBS_CENSORED_NUMERIC))
})

test_that("stop_fmt_if gives a non-symbol template an NA id and keeps only named fields", {
  err <- tryCatch(stop_fmt_if(TRUE, "found %d errors", 3L), error = identity)
  expect_s3_class(err, "imugap_error")
  expect_identical(err$id, NA_character_)
  expect_identical(conditionMessage(err), "found 3 errors")
  expect_named(unclass(err), c("message", "call", "id"))
})
