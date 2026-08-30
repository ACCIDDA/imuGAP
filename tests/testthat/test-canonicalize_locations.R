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
    parent_id = c(NA, "a", "a", "b", "b")
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
  expect_equal(locs$loc_id, c("a", "b", "c", "d", "e"))
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

test_that("canonicalize_locations errors when a location has exactly 1 offspring", {
  # Direct single child of root (simple 2-node chain)
  locs_single_child <- data.frame(
    loc_id = c("state", "cnty1"),
    parent_id = c(NA, "state")
  )
  expect_error(
    canonicalize_locations(locs_single_child),
    "either 0 or strictly greater than 1 offspring.*state"
  )

  # Intermediate node with single child in a branch
  locs_single_grandchild <- data.frame(
    loc_id = c("state", "cnty1", "cnty2", "schl1"),
    parent_id = c(NA, "state", "state", "cnty1")
  )
  expect_error(
    canonicalize_locations(locs_single_grandchild),
    "either 0 or strictly greater than 1 offspring.*cnty1"
  )

  # Linear chain hierarchy: A -> B -> C -> D
  locs_chain <- data.frame(
    loc_id = c("A", "B", "C", "D"),
    parent_id = c(NA, "A", "B", "C")
  )
  expect_error(
    canonicalize_locations(locs_chain),
    "either 0 or strictly greater than 1 offspring"
  )
})

test_that("canonicalize_locations supports 1-layer (single root) hierarchy", {
  locs1 <- data.frame(
    loc_id = "state",
    parent_id = NA
  )
  res <- canonicalize_locations(locs1)
  expect_true(data.table::is.data.table(res))
  expect_equal(nrow(res), nrow(locs1))
  expect_equal(res$loc_id, locs1$loc_id)
  expect_equal(res$layer, 1L)
  expect_equal(res$loc_c_id, seq_len(nrow(locs1)))
  expect_true(is.na(res$loc_cp_id))
  expect_equal(res$layer_bound, 1L)
})

test_that("canonicalize_locations supports 2-layer hierarchy", {
  locs2 <- data.frame(
    loc_id = c("state", "cnty1", "cnty2"),
    parent_id = c(NA, "state", "state")
  )
  res <- canonicalize_locations(locs2)
  expect_equal(nrow(res), nrow(locs2))
  expect_equal(res$layer, c(1L, rep(2L, nrow(locs2) - 1L)))
  expect_equal(res$loc_id, locs2$loc_id)
  expect_equal(res$loc_c_id, seq_len(nrow(locs2)))
  expect_equal(res$loc_cp_id, c(NA_integer_, rep(1L, nrow(locs2) - 1L)))
  expect_equal(res$layer_bound, c(1L, rep(1L, nrow(locs2) - 1L)))
})

test_that("canonicalize_locations supports 4-layer and 5-layer hierarchies", {
  # 4-layer hierarchy: state (1) -> county (2) -> district (2 under cnty1) -> school (2 under dist1)
  locs4 <- data.frame(
    loc_id = c(
      "state",
      "cnty1",
      "cnty2",
      "dist1",
      "dist2",
      "schl1",
      "schl2"
    ),
    parent_id = c(
      NA,
      "state",
      "state",
      "cnty1",
      "cnty1",
      "dist1",
      "dist1"
    )
  )
  res4 <- canonicalize_locations(locs4)
  expect_equal(nrow(res4), nrow(locs4))
  expect_equal(max(res4$layer), 4L)
  expect_equal(res4$loc_c_id, seq_len(nrow(locs4)))
  expect_equal(
    res4[loc_id == "dist1", loc_cp_id],
    res4[loc_id == "cnty1", loc_c_id]
  )
  expect_equal(
    res4[loc_id == "dist2", loc_cp_id],
    res4[loc_id == "cnty1", loc_c_id]
  )
  expect_equal(
    res4[loc_id == "schl1", loc_cp_id],
    res4[loc_id == "dist1", loc_c_id]
  )
  expect_equal(
    res4[loc_id == "schl2", loc_cp_id],
    res4[loc_id == "dist1", loc_c_id]
  )

  # 5-layer hierarchy: country -> region -> state -> county -> school
  # (each branching node has >= 2 offspring)
  locs5 <- data.frame(
    loc_id = c(
      "USA",
      "East",
      "West",
      "NC",
      "VA",
      "Wake",
      "Orange",
      "Enloe",
      "Broughton"
    ),
    parent_id = c(
      NA,
      "USA",
      "USA",
      "East",
      "East",
      "NC",
      "NC",
      "Wake",
      "Wake"
    )
  )
  res5 <- canonicalize_locations(locs5)
  expect_equal(nrow(res5), nrow(locs5))
  expect_equal(max(res5$layer), 5L)
  expect_equal(res5$loc_c_id, seq_len(nrow(locs5)))
  expect_equal(
    res5[loc_id == "Enloe", loc_cp_id],
    res5[loc_id == "Wake", loc_c_id]
  )
})

test_that("canonicalize_locations imputes NA root population from children sum", {
  locs_pop <- data.frame(
    loc_id = c("state", "cnty1", "cnty2"),
    parent_id = c(NA, "state", "state"),
    population = c(NA, 100, 200)
  )
  res <- canonicalize_locations(locs_pop)
  expect_equal(res[loc_id == "state", population], 300)
})

test_that("canonicalize_locations validates consistent multi-layer population sums", {
  locs_pop <- data.frame(
    loc_id = c("state", "cnty1", "cnty2", "schl1", "schl2"),
    parent_id = c(NA, "state", "state", "cnty1", "cnty1"),
    population = c(300, 100, 200, 60, 40)
  )
  expect_silent(res <- canonicalize_locations(locs_pop))
  expect_equal(res[loc_id == "state", population], 300)
  expect_equal(res[loc_id == "cnty1", population], 100)
})

test_that("canonicalize_locations errors when child populations do not sum to parent", {
  locs_bad_pop <- data.frame(
    loc_id = c("state", "cnty1", "cnty2", "schl1", "schl2"),
    parent_id = c(NA, "state", "state", "cnty1", "cnty1"),
    population = c(300, 100, 200, 60, 50)
  )
  expect_error(
    canonicalize_locations(locs_bad_pop),
    "populations for parent 'cnty1' sum to 110"
  )
})
