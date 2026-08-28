library(data.table)

test_that("stop_fmt_if correctly attributes call context", {
  inner_func <- function() {
    stop_fmt_if(TRUE, "test error value %d", 42, n = 1L)
  }
  outer_func <- function() {
    inner_func()
  }
  inner_func_n2 <- function() {
    stop_fmt_if(TRUE, "test error value %d", 42, n = 2L)
  }
  outer_func_n2 <- function() {
    inner_func_n2()
  }
  no_call_func <- function() {
    stop_fmt_if(TRUE, "test error value %d", 42, n = 0L)
  }

  err1 <- tryCatch(outer_func(), error = identity)
  expect_s3_class(err1, "error")
  expect_equal(deparse(err1$call), "inner_func()")
  expect_equal(err1$message, "test error value 42")

  err2 <- tryCatch(outer_func_n2(), error = identity)
  expect_s3_class(err2, "error")
  expect_equal(deparse(err2$call), "outer_func_n2()")

  err0 <- tryCatch(no_call_func(), error = identity)
  expect_s3_class(err0, "error")
  expect_null(err0$call)
})

test_that("stop_fmt_if conditionally stops with call context", {
  test_func <- function(x) {
    stop_fmt_if(x > 10, "value %d is too large", x)
    x * 2
  }

  expect_equal(test_func(5), 10)

  err <- tryCatch(test_func(15), error = identity)
  expect_s3_class(err, "error")
  expect_equal(deparse(err$call), "test_func(15)")
  expect_equal(err$message, "value 15 is too large")
})

test_that("warn_fmt_if conditionally warns with call context and returns boolean", {
  test_func <- function(x) {
    warned <- warn_fmt_if(x > 10, "value %d is large", x, n = 1L)
    list(result = x * 2, warned = warned)
  }

  # Return FALSE and no warning when condition is FALSE
  res_false <- expect_silent(test_func(5))
  expect_equal(res_false$result, 10)
  expect_identical(res_false$warned, FALSE)

  # Return TRUE and emit warning when condition is TRUE
  warn1 <- tryCatch(test_func(15), warning = identity)
  expect_s3_class(warn1, "warning")
  expect_equal(deparse(warn1$call), "test_func(15)")
  expect_equal(warn1$message, "value 15 is large")

  # Direct return value evaluation with expect_warning
  expect_warning(res_true <- warn_fmt_if(TRUE, "msg"), "msg")
  expect_identical(res_true, TRUE)
  expect_identical(expect_silent(warn_fmt_if(FALSE, "msg")), FALSE)
  expect_identical(expect_silent(warn_fmt_if(NA, "msg")), FALSE)
  expect_identical(expect_silent(warn_fmt_if(NULL, "msg")), FALSE)

  # Control flow guard pattern: if (warn_fmt_if(...))
  branched <- FALSE
  expect_warning({
    if (warn_fmt_if(TRUE, "branch warning")) {
      branched <- TRUE
    }
  }, "branch warning")
  expect_true(branched)

  branched <- FALSE
  expect_silent({
    if (warn_fmt_if(FALSE, "branch warning")) {
      branched <- TRUE
    }
  })
  expect_false(branched)

  no_call_func <- function() {
    warn_fmt_if(TRUE, "no call warning", n = 0L)
  }
  warn0 <- tryCatch(no_call_func(), warning = identity)
  expect_null(warn0$call)
})

test_that("assert_as_integer works", {
  ref <- c(1.0, 2.0, 3.0)
  ref_dt <- data.table(
    a = as.integer(ref), # integers
    b = ref, # coercible to integers
    c = ref + 0.5 # not coercible
  )

  expect_silent(assert_as_integer(ref_dt, "a"))
  expect_equal(ref_dt$a, as.integer(ref))

  expect_silent(assert_as_integer(ref_dt, "b"))
  expect_equal(class(ref_dt$b), "integer")

  expect_error(
    assert_as_integer(ref_dt, "c"),
    "ref_dt.*'c'"
  )

  somefun <- function(some_dt, col) {
    eval(substitute(assert_as_integer(some_dt, col)))
  }

  expect_silent(somefun(ref_dt, "a"))
  expect_silent(somefun(ref_dt, "b"))
  expect_error(
    somefun(ref_dt, "c"),
    "ref_dt.*'c'"
  )
})

test_that("assert_positive_integer works", {
  ref <- c(1.0, 2.0, 3.0)
  ref_dt <- data.table(
    a = ref, # still coercible to integers
    b = -as.integer(ref) # error: negative integers
  )

  expect_silent(assert_positive_integer(ref_dt, "a"))
  expect_equal(ref_dt$a, as.integer(ref))
  expect_equal(class(ref_dt$a), "integer")

  expect_error(
    assert_positive_integer(ref_dt, "b"),
    "ref_dt.*'b'"
  )

  somefun <- function(some_dt, col) {
    eval(substitute(assert_positive_integer(some_dt, col)))
  }

  expect_silent(somefun(ref_dt, "a"))
})

test_that("assert_maxed_pos_integer works", {
  ref <- c(1.0, 2.0, 3.0, 4.0, 5.0)
  ref_dt <- data.table(
    a = ref, # still coercible to integers
    b = ref + 10.0 # error: will exceed max
  )

  expect_silent(assert_maxed_pos_integer(ref_dt, "a", 5))
  expect_equal(ref_dt$a, as.integer(ref))
  expect_equal(class(ref_dt$a), "integer")

  expect_error(
    assert_maxed_pos_integer(ref_dt, "b", 5),
    "ref_dt.*'b'"
  )

  expect_silent(assert_maxed_pos_integer(ref_dt, "b"))

  somefun <- function(some_dt, col, max) {
    eval(substitute(assert_maxed_pos_integer(some_dt, col, max)))
  }

  expect_silent(somefun(ref_dt, "a", 5))
  expect_silent(somefun(ref_dt, "a"))
})

test_that("assert_set_equivalence works", {
  refset <- c(1L, 2L, 3L)

  ref_dt <- data.table(
    a = rep(refset, 2), # extras
    b = head(refset, -1), # missing
    d = refset + 1.0 # extras
  )

  expect_silent(assert_set_equivalence(ref_dt, "a", refset))
  expect_error(
    assert_set_equivalence(ref_dt, "b", refset),
    "ref_dt.*'b'"
  )
  expect_error(
    assert_set_equivalence(ref_dt, "d", refset),
    "ref_dt.*'d'"
  )
})

test_that("assert_set_equivalence flags values outside the set", {
  # Column 'c' contains all refset members AND extras (4) — triggers the
  # "values outside of set" branch (i.e. union > setlen but intersect == setlen).
  refset <- c(1L, 2L, 3L)
  ref_dt <- data.table(
    c = c(refset, 4L)
  )
  expect_error(
    assert_set_equivalence(ref_dt, "c", refset),
    "outside permitted set"
  )
})

test_that("assert_as_integer errors on NA when na_allowed = FALSE", {
  ref_dt <- data.table(a = c(1L, NA_integer_, 3L))
  expect_error(
    assert_as_integer(ref_dt, "a"),
    "cannot contain NA"
  )
})

test_that("assert_as_integer allows NA when na_allowed = TRUE", {
  ref_dt <- data.table(a = c(1L, NA_integer_, 3L))
  expect_silent(assert_as_integer(ref_dt, "a", na_allowed = TRUE))
})

test_that("assert_subset accepts column whose values are all in tarset", {
  ref_dt <- data.table(a = c(1L, 2L, 1L))
  expect_silent(assert_subset(ref_dt, "a", c(1L, 2L, 3L)))
})

test_that("assert_subset errors when column has values outside tarset", {
  ref_dt <- data.table(a = c(1L, 2L, 99L))
  expect_error(
    assert_subset(ref_dt, "a", c(1L, 2L, 3L)),
    "parent set"
  )
})

test_that("assert_subset error message names missing values", {
  ref_dt <- data.table(a = c(1L, 7L, 8L))
  expect_error(
    assert_subset(ref_dt, "a", c(1L, 2L, 3L)),
    "7"
  )
})

test_that("assert_dt_able returns data.table via setDT when copy = FALSE", {
  df <- data.frame(a = 1:3, b = 4:6)
  res <- assert_dt_able(df, copy = FALSE)
  expect_s3_class(res, "data.table")
})

test_that("assert_dt_able returns data.table via as.data.table when copy = TRUE", {
  df <- data.frame(a = 1:3, b = 4:6)
  res <- assert_dt_able(df, copy = TRUE)
  expect_s3_class(res, "data.table")
  # Should be a fresh copy, not modify df
  expect_s3_class(df, "data.frame")
  expect_false(data.table::is.data.table(df))
})

test_that("assert_positive_numeric validates numeric type, NA presence, and bounds", {
  ref_dt <- data.table(
    a = c(1.5, 2.3, 3.1),
    b = c(1.5, NA_real_, 3.1),
    c = c(1.5, -0.5, 3.1),
    d = c("x", "y", "z")
  )

  # Valid calls
  expect_silent(assert_positive_numeric(ref_dt, "a"))

  # NA error check
  expect_error(
    assert_positive_numeric(ref_dt, "b"),
    "cannot contain NA values"
  )

  # Non-positive check
  expect_error(
    assert_positive_numeric(ref_dt, "c"),
    "must contain values > 0"
  )

  # Non-numeric check
  expect_error(
    assert_positive_numeric(ref_dt, "d"),
    "must contain numeric values"
  )
})
