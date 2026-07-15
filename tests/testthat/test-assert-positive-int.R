# assert_positive_int() is the one backend helper imuGAP kept locally after the
# flexstanr migration (#122); the rest moved to flexstanr. (flexstanr covers its
# own copy; this guards imuGAP's.)

test_that("assert_positive_int coerces valid input to integer", {
  expect_identical(assert_positive_int(4, "x"), 4L)
  expect_identical(assert_positive_int(c(1, 2, 3), "x"), c(1L, 2L, 3L))
})

test_that("assert_positive_int rejects invalid input", {
  expect_error(assert_positive_int("3", "x"), "numeric")
  expect_error(assert_positive_int(integer(0), "x"), ">= 1")
  expect_error(assert_positive_int(c(1, NA), "x"), "NAs")
  expect_error(assert_positive_int(1.5, "x"), "integers")
  expect_error(assert_positive_int(0L, "x"), "positive")
})
