test_that("can enforce id and parent_id columns", {
  # valid locations data should pass without error or warning
  expect_silent(
    canonicalize_locations(data.frame(loc_id = 1:3, parent_id = c(NA, 1, 1)))
  )

  expect_silent(
    canonicalize_locations(data.frame(
      loc_id = c("a", "b", "c"),
      parent_id = c(NA, "a", "a")
    ))
  )

  expect_error(
    canonicalize_locations(data.frame(parent_id = c(NA, 1, 2))),
    "loc_id"
  )

  expect_error(
    canonicalize_locations(data.frame(loc_id = 1:3)),
    "parent_id"
  )

  expect_warning(
    canonicalize_locations(data.frame(
      loc_id = 1:3,
      parent_id = c(NA, 1, 1),
      extra_col = "x"
    )),
    "extra_col"
  )
})

test_that("can enforce unique ids", {
  expect_error(
    canonicalize_locations(data.frame(
      loc_id = c(1, 1, 2),
      parent_id = c(NA, 1, 1)
    ))
  )
})

test_that("can enforce unique root", {
  expect_error(
    canonicalize_locations(data.frame(loc_id = 1:3, parent_id = c(NA, NA, 1))),
    "one root.*2"
  )
  expect_error(
    canonicalize_locations(data.frame(loc_id = 1:3, parent_id = c(2, 3, 1))),
    "one root.*0"
  )
})

test_that("can enforce no cycles", {
  expect_error(
    canonicalize_locations(data.frame(
      loc_id = 1:4,
      parent_id = c(NA, 1, 4, 3)
    )),
    "cycle"
  )
})

test_that("yields data.table with ordered layer, parent_id, and id columns", {
  ref <- data.frame(
    loc_id = c("a", "b", "c", "d", "e"),
    parent_id = c(NA, "a", "a", "c", "b")
  )

  locs <- canonicalize_locations(ref)

  expect_true(data.table::is.data.table(locs))
  expect_equal(
    names(locs),
    c(
      names(ref),
      "layer",
      "loc_c_id",
      "loc_cp_id",
      "layer_bound"
    )
  )
  expect_equal(locs$layer, c(1L, 2L, 2L, 3L, 3L))
  expect_equal(locs$loc_id, c("a", "b", "c", "e", "d"))
  expect_equal(locs$loc_c_id, sort(locs$loc_c_id, na.last = FALSE))
  expect_equal(locs$loc_cp_id, sort(locs$loc_cp_id, na.last = FALSE))
})

test_that("infers implicit root when no row has parent_id == NA", {
  res <- canonicalize_locations(make_test_locs_implicit_root())
  root_rows <- res[res$layer == 1L, ]
  expect_equal(nrow(root_rows), 1L)
  expect_equal(root_rows$loc_id, "root")
})

test_that("canonical input short-circuits and returns unchanged", {
  canon <- canonicalize_locations(make_test_locs())
  again <- canonicalize_locations(canon)
  expect_identical(canon, again)
})

test_that("validates population hierarchy when population column is present", {
  # Valid population sum (State=100, County A=60, County B=40, School A1=40, School A2=20, School B1=40)
  valid_locs <- data.frame(
    loc_id = c("State", "A", "B", "A1", "A2", "B1"),
    parent_id = c(NA, "State", "State", "A", "A", "B"),
    population = c(100, 60, 40, 40, 20, 40)
  )
  expect_silent(canonicalize_locations(valid_locs))

  # Mismatched population sum (A1=40, A2=30 sum to 70 != A=60)
  invalid_locs <- data.frame(
    loc_id = c("State", "A", "B", "A1", "A2", "B1"),
    parent_id = c(NA, "State", "State", "A", "A", "B"),
    population = c(100, 60, 40, 40, 30, 40)
  )
  expect_error(
    canonicalize_locations(invalid_locs),
    "Child location populations for parent 'A' sum to 70"
  )
})
