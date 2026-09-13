test_that("assemble_layer_data handles 2-layer hierarchy", {
  locs2 <- canonicalize_locations(data.frame(
    loc_id = c("state", "cnty1", "cnty2"),
    parent_id = c(NA, "state", "state")
  ))
  d2 <- assemble_layer_data(locs2)

  expect_equal(d2$n_locs, nrow(locs2))
  expect_equal(d2$n_layers, max(locs2$layer))
  expect_equal(
    as.integer(d2$layer_starts),
    as.integer(locs2[, min(loc_c_id), by = layer]$V1)
  )
  expect_equal(
    d2$n_parent_locs,
    length(unique(locs2$parent_id[!is.na(locs2$parent_id)]))
  )
  expect_equal(as.integer(d2$parent_loc_id), locs2[loc_id == "state", loc_c_id])
  expect_equal(as.integer(d2$parent_child_starts), 2L)
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
    as.integer(d3$layer_starts),
    as.integer(locs3[, min(loc_c_id), by = layer]$V1)
  )
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
  expect_equal(as.integer(d3$parent_child_starts), c(2L, 4L, 6L))
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
    as.integer(d4$layer_starts),
    as.integer(locs4[, min(loc_c_id), by = layer]$V1)
  )
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
  expect_equal(as.integer(d4$parent_child_starts), c(2L, 4L, 6L))
  expect_equal(d4$sigma_layer_scale, 2.5)
})

test_that("assemble_layer_data respects custom sigma_layer_scale", {
  locs <- canonicalize_locations(data.frame(
    loc_id = c("state", "cnty1", "cnty2"),
    parent_id = c(NA, "state", "state")
  ))
  d <- assemble_layer_data(locs, sigma_layer_scale = 1.2)
  expect_equal(d$sigma_layer_scale, 1.2)
})

test_that("assemble_layer_data handles 1-layer (root-only) hierarchy", {
  locs1 <- canonicalize_locations(data.frame(
    loc_id = "state",
    parent_id = NA_character_
  ))
  d1 <- assemble_layer_data(locs1)
  expect_equal(d1$n_locs, 1L)
  expect_equal(d1$n_layers, 1L)
  expect_equal(d1$n_parent_locs, 0L)
  expect_equal(length(d1$parent_loc_id), 0L)
  expect_equal(length(d1$parent_child_starts), 0L)
})
