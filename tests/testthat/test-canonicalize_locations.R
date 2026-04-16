
test_that("can enforce id and parent_id columns", {

  # valid locations data should pass without error or warning
  expect_silent(
    canonicalize_locations(data.frame(id = 1:3, parent_id = c(NA, 1, 1)))
  )

  expect_silent(
    canonicalize_locations(data.frame(
      id = c("a", "b", "c"), parent_id = c(NA, "a", "a")
    ))
  )

  expect_error(
    canonicalize_locations(data.frame(parent_id = c(NA, 1, 2))),
    "id"
  )

  expect_error(
    canonicalize_locations(data.frame(id = 1:3)),
    "parent_id"
  )

  expect_warning(
    canonicalize_locations(data.frame(
      id = 1:3, parent_id = c(NA, 1, 1), extra_col = "x"
    )),
    "extra_col"
  )
})

test_that("can enforce unique ids", {
  expect_error(
    canonicalize_locations(data.frame(id = c(1, 1, 2), parent_id = c(NA, 1, 1)))
  )
})

test_that("can enforce unique root", {
  expect_error(
    canonicalize_locations(data.frame(id = 1:3, parent_id = c(NA, NA, 1))),
    "one root.*2"
  )
  expect_error(
    canonicalize_locations(data.frame(id = 1:3, parent_id = c(2, 3, 1))),
    "one root.*0"
  )
})

test_that("can enforce no cycles", {
  expect_error(
    canonicalize_locations(data.frame(id = 1:4, parent_id = c(NA, 1, 4, 3))),
    "cycle"
  )
})

test_that("yields data.table with ordered layer, parent_id, and id columns", {

  locs <- canonicalize_locations(data.frame(
    id = c("a", "b", "c", "d", "e"),
    parent_id = c(NA, "a", "a", "c", "b")
  ))

  expect_true(data.table::is.data.table(locs))
  expect_equal(names(locs), c("id", "parent_id", "layer", "c_id", "cp_id"))
  expect_equal(locs$layer, c(1L, 2L, 2L, 3L, 3L))
  expect_equal(locs$id, c("a", "b", "c", "e", "d"))
  expect_equal(locs$c_id, sort(locs$c_id, na.last = FALSE))
  expect_equal(locs$cp_id, sort(locs$cp_id, na.last = FALSE))

})