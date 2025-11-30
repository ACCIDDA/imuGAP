
library(data.table)

test_that("checked_as_integer works", {
  ref <- c(1.0, 2.0, 3.0)
  ref_dt <- data.table(
    a = as.integer(ref), # integers
    b = ref, # coercible to integers
    c = ref + 0.5 # not coercible
  )

  expect_silent(checked_as_integer(ref_dt, "a"))
  expect_equal(ref_dt$a, as.integer(ref))

  expect_silent(checked_as_integer(ref_dt, "b"))
  expect_equal(class(ref_dt$b), "integer")

  expect_error(
    checked_as_integer(ref_dt, "c"),
    "'ref_dt'.*'c'"
  )

  somefun <- function(DT, col) eval(substitute(checked_as_integer(DT, col)))

  expect_silent(somefun(ref_dt, "a"))
  expect_silent(somefun(ref_dt, "b"))
  expect_error(
    somefun(ref_dt, "c"),
    "'ref_dt'.*'c'"
  )

})

test_that("checked_positive_integer works", {
  ref <- c(1.0, 2.0, 3.0)
  ref_dt <- data.table(
    a = ref, # still coercible to integers
    b = -as.integer(ref) # error: negative integers
  )

  expect_silent(checked_positive_integer(ref_dt, "a"))
  expect_equal(ref_dt$a, as.integer(ref))
  expect_equal(class(ref_dt$a), "integer")

  expect_error(
    checked_positive_integer(ref_dt, "b"),
    "'ref_dt'.*'b'"
  )

  somefun <- function(DT, col) eval(
    substitute(checked_positive_integer(DT, col))
  )

  expect_silent(somefun(ref_dt, "a"))

})

test_that("checked_maxed_pos_integer works", {
  ref <- c(1.0, 2.0, 3.0, 4.0, 5.0)
  ref_dt <- data.table(
    a = ref, # still coercible to integers
    b = ref + 10.0 # error: will exceed max
  )

  expect_silent(checked_maxed_pos_integer(ref_dt, "a", 5))
  expect_equal(ref_dt$a, as.integer(ref))
  expect_equal(class(ref_dt$a), "integer")

  expect_error(
    checked_maxed_pos_integer(ref_dt, "b", 5),
    "'ref_dt'.*'b'"
  )

  expect_silent(checked_maxed_pos_integer(ref_dt, "b"))

  somefun <- function(DT, col, max) eval(
    substitute(checked_maxed_pos_integer(DT, col, max))
  )

  expect_silent(somefun(ref_dt, "a", 5))
  expect_silent(somefun(ref_dt, "a"))

})

test_that("checked_set_equivalence works", {
  refset <- c(1L, 2L, 3L)

  ref_dt <- data.table(
    a = rep(refset, 2), # extras
    b = head(refset, -1), # missing
    d = refset + 1.0 # extras
  )

  expect_silent(checked_set_equivalence(ref_dt, "a", refset))
  expect_error(
    checked_set_equivalence(ref_dt, "b", refset),
    "'ref_dt'.*'b'"
  )
  expect_error(
    checked_set_equivalence(ref_dt, "d", refset),
    "'ref_dt'.*'d'"
  )
})

# test_that("checked_dt_able works", {
#   dt <- data.frame(a = 1:3, b = 4:6)

#   expect_silent(checked_dt_able(dt))
#   expect_s3_class(dt, "data.table")

#   list_dt <- list(a = 1:3, b = 4:6)
#   expect_silent(checked_dt_able(list_dt))
#   expect_s3_class(list_dt, "data.table")

# })

# test_that("check_obs works for obvious case", {
#   dt <- data.table(
#     obs_id = 1:3,
#     location = 1:3,
#     cohort = 1:3,
#     age = rep(10, 3),
#     dose = rep(1, 3),
#     weight = rep(1.0, 3)
#   )

#   expect_silent(
#     res <- check_obs_population(
#       dt,
#       max_obs_id = 3,
#       max_location = 3,
#       max_cohort = 3,
#       max_age = 30
#     )
#   )

#   expect_equal(res, dt)
# })
