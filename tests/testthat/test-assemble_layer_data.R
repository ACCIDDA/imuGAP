test_that("assemble_layer_data handles 1-layer (single root) edge case", {
  locs1 <- canonicalize_locations(data.frame(
    loc_id = "state",
    parent_id = NA
  ))
  d1 <- assemble_layer_data(locs1)

  expect_equal(d1$n_locs, nrow(locs1))
  expect_equal(d1$n_layers, max(locs1$layer))
  expect_equal(
    as.integer(d1$layer_sizes),
    as.integer(locs1[, .N, keyby = layer]$N)
  )
  expect_equal(d1$layer_bounds, matrix(c(1L, 1L), nrow = 2, ncol = 1))
  expect_equal(as.integer(d1$parent_id_map), 0L)
  expect_equal(as.integer(d1$layer_id_map), locs1$layer)
  expect_equal(d1$n_parent_locs, 0L)
  expect_equal(length(d1$parent_loc_id), 0L)
  expect_equal(dim(d1$parent_child_bounds), c(2L, 0L))
})

test_that("assemble_layer_data handles 2-layer hierarchy", {
  locs2 <- canonicalize_locations(data.frame(
    loc_id = c("state", "cnty1", "cnty2"),
    parent_id = c(NA, "state", "state")
  ))
  d2 <- assemble_layer_data(locs2)

  expect_equal(d2$n_locs, nrow(locs2))
  expect_equal(d2$n_layers, max(locs2$layer))
  expect_equal(
    as.integer(d2$layer_sizes),
    as.integer(locs2[, .N, keyby = layer]$N)
  )
  expect_equal(
    d2$layer_bounds,
    matrix(c(1L, 1L, 2L, nrow(locs2)), nrow = 2, ncol = max(locs2$layer))
  )
  expect_equal(as.integer(d2$parent_id_map), c(0L, rep(1L, nrow(locs2) - 1L)))
  expect_equal(as.integer(d2$layer_id_map), locs2$layer)
  expect_equal(
    d2$n_parent_locs,
    length(unique(locs2$parent_id[!is.na(locs2$parent_id)]))
  )
  expect_equal(as.integer(d2$parent_loc_id), locs2[loc_id == "state", loc_c_id])
  expect_equal(
    d2$parent_child_bounds,
    matrix(c(2L, nrow(locs2)), nrow = 2, ncol = 1)
  )
})

test_that("assemble_layer_data handles 3-layer branching hierarchy", {
  locs3 <- canonicalize_locations(data.frame(
    loc_id = c("state", "c1", "c2", "s1", "s2", "s3", "s4"),
    parent_id = c(NA, "state", "state", "c1", "c1", "c2", "c2")
  ))
  d3 <- assemble_layer_data(locs3)

  expect_equal(d3$n_locs, nrow(locs3))
  expect_equal(d3$n_layers, max(locs3$layer))
  expect_equal(
    as.integer(d3$layer_sizes),
    as.integer(locs3[, .N, keyby = layer]$N)
  )
  expect_equal(as.integer(d3$layer_id_map), locs3$layer)
  expect_equal(
    d3$n_parent_locs,
    length(unique(locs3$parent_id[!is.na(locs3$parent_id)]))
  )
  expect_equal(
    as.integer(d3$parent_loc_id),
    locs3[
      loc_id %in% unique(locs3$parent_id[!is.na(locs3$parent_id)]),
      loc_c_id
    ]
  )
})

test_that("assemble_layer_data handles 4-layer deep hierarchy", {
  locs4 <- canonicalize_locations(data.frame(
    loc_id = c("state", "c1", "c2", "d1", "d2", "s1", "s2"),
    parent_id = c(NA, "state", "state", "c1", "c1", "d1", "d1")
  ))
  d4 <- assemble_layer_data(locs4)

  expect_equal(d4$n_locs, nrow(locs4))
  expect_equal(d4$n_layers, max(locs4$layer))
  expect_equal(
    as.integer(d4$layer_sizes),
    as.integer(locs4[, .N, keyby = layer]$N)
  )
  expect_equal(as.integer(d4$layer_id_map), locs4$layer)
  expect_equal(
    d4$n_parent_locs,
    length(unique(locs4$parent_id[!is.na(locs4$parent_id)]))
  )
  expect_equal(
    as.integer(d4$parent_loc_id),
    locs4[
      loc_id %in% unique(locs4$parent_id[!is.na(locs4$parent_id)]),
      loc_c_id
    ]
  )
})
