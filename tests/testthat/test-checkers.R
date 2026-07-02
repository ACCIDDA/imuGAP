library(data.table)

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
    "'ref_dt'.*'c'"
  )

  somefun <- function(some_dt, col) {
    eval(substitute(assert_as_integer(some_dt, col)))
  }

  expect_silent(somefun(ref_dt, "a"))
  expect_silent(somefun(ref_dt, "b"))
  expect_error(
    somefun(ref_dt, "c"),
    "'ref_dt'.*'c'"
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
    "'ref_dt'.*'b'"
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
    "'ref_dt'.*'b'"
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
    "'ref_dt'.*'b'"
  )
  expect_error(
    assert_set_equivalence(ref_dt, "d", refset),
    "'ref_dt'.*'d'"
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
    "outside of set"
  )
})

test_that("assert_as_integer errors on NA when na_allowed = FALSE", {
  ref_dt <- data.table(a = c(1L, NA_integer_, 3L))
  expect_error(
    assert_as_integer(ref_dt, "a"),
    "cannot have NA"
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
