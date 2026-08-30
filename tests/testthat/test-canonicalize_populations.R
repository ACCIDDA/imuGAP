test_that("canonicalize_populations succeeds on valid input", {
  res <- canonicalize_populations(
    make_test_pops(),
    make_test_obs(),
    make_test_locs(),
    max_cohort = 5L,
    max_age = 10L
  )
  expect_s3_class(res, "data.table")
  expect_true(all(c("obs_c_id", "loc_c_id", "range_start") %in% names(res)))
  expect_equal(nrow(res), 2L)
  expect_true(all(!is.na(res$obs_c_id)))
  expect_true(all(!is.na(res$loc_c_id)))
})

test_that("canonicalize_populations short-circuits on canonical input", {
  res1 <- canonicalize_populations(
    make_test_pops(),
    make_test_obs(),
    make_test_locs(),
    max_cohort = 5L,
    max_age = 10L
  )
  # second pass should pass through unchanged (no validation, no error)
  res2 <- canonicalize_populations(
    res1,
    make_test_obs(),
    make_test_locs(),
    max_cohort = 5L,
    max_age = 10L
  )
  expect_identical(res1, res2)
})

test_that("canonicalize_populations assigns range_start by obs_c_id", {
  base <- make_test_pops()
  pops <- rbind(
    transform(base, dose = 1L, weight = c(0.5, 0.6)),
    transform(base, dose = 2L, weight = c(0.5, 0.4))
  )
  res <- canonicalize_populations(
    pops,
    make_test_obs(),
    make_test_locs(),
    max_cohort = 5L,
    max_age = 10L
  )
  expect_equal(nrow(res), 4L)
  starts <- res[, .(start = unique(range_start)), by = obs_c_id]
  expect_equal(nrow(starts), 2L)
})

# --- error paths -------------------------------------------------------------

test_that("canonicalize_populations errors on missing required columns", {
  # With duplicate obs_id, weight is required
  bad <- rbind(make_test_pops(), make_test_pops()[1, ])
  bad$weight <- NULL
  expect_error(
    canonicalize_populations(
      bad,
      make_test_obs(),
      make_test_locs(),
      max_cohort = 5L,
      max_age = 10L
    ),
    "weight"
  )
})

test_that("canonicalize_populations infers weight = 1.0 when missing and obs_ids are unique", {
  pops <- make_test_pops()
  pops$weight <- NULL
  res <- canonicalize_populations(
    pops,
    make_test_obs(),
    make_test_locs(),
    max_cohort = 5L,
    max_age = 10L
  )
  expect_s3_class(res, "data.table")
  expect_true("weight" %in% names(res))
  expect_true(all(res$weight == 1.0))
})

test_that("canonicalize_populations errors on dose outside {1, 2}", {
  bad <- make_test_pops()
  bad$dose <- c(1L, 3L)
  expect_error(
    canonicalize_populations(
      bad,
      make_test_obs(),
      make_test_locs(),
      max_cohort = 5L,
      max_age = 10L
    ),
    "dose"
  )
})

test_that("canonicalize_populations errors when obs_id does not cover all observations", {
  bad <- make_test_pops()
  # Drop o2 so populations only covers o1
  bad <- bad[bad$obs_id != "o2", ]
  expect_error(
    canonicalize_populations(
      bad,
      make_test_obs(),
      make_test_locs(),
      max_cohort = 5L,
      max_age = 10L
    ),
    "obs_id"
  )
})

test_that("canonicalize_populations errors on loc_id not in locations", {
  bad <- make_test_pops()
  bad$loc_id <- c("schl", "nowhere")
  expect_error(
    canonicalize_populations(
      bad,
      make_test_obs(),
      make_test_locs(),
      max_cohort = 5L,
      max_age = 10L
    ),
    "loc_id"
  )
})

test_that("canonicalize_populations errors on cohort > max_cohort", {
  bad <- make_test_pops()
  bad$cohort <- c(1L, 99L)
  expect_error(
    canonicalize_populations(
      bad,
      make_test_obs(),
      make_test_locs(),
      max_cohort = 5L,
      max_age = 10L
    ),
    "cohort"
  )
})

test_that("canonicalize_populations errors on age > max_age", {
  bad <- make_test_pops()
  bad[2, "age"] <- 99L
  expect_error(
    canonicalize_populations(
      bad,
      make_test_obs(),
      make_test_locs(),
      max_cohort = 5L,
      max_age = 10L
    ),
    "age"
  )
})

test_that("canonicalize_populations errors on non-positive or NA weight", {
  bad <- make_test_pops()
  bad$weight <- c(1.0, 0.0)
  expect_error(
    canonicalize_populations(
      bad,
      make_test_obs(),
      make_test_locs(),
      max_cohort = 5L,
      max_age = 10L
    ),
    "weight"
  )
  bad2 <- make_test_pops()
  bad2$weight <- c(1.0, -0.5)
  expect_error(
    canonicalize_populations(
      bad2,
      make_test_obs(),
      make_test_locs(),
      max_cohort = 5L,
      max_age = 10L
    ),
    "weight"
  )
  bad_na <- make_test_pops()
  bad_na$weight <- c(1.0, NA_real_)
  expect_error(
    canonicalize_populations(
      bad_na,
      make_test_obs(),
      make_test_locs(),
      max_cohort = 5L,
      max_age = 10L
    ),
    "weight"
  )
})

test_that("canonicalize_populations errors when weights do not sum to 1 by obs_id", {
  bad <- rbind(
    make_test_pops(),
    data.frame(
      obs_id = "o1",
      loc_id = "schl1",
      cohort = 1L,
      age = 2L,
      dose = 2L,
      weight = 0.3
    )
  )
  expect_error(
    canonicalize_populations(
      bad,
      make_test_obs(),
      make_test_locs(),
      max_cohort = 5L,
      max_age = 10L
    ),
    "sum to 1"
  )
})

test_that("canonicalize_populations succeeds with bundled simulated data", {
  # Sanity check: the bundled fixtures should round-trip.
  data(populations_sim, package = "imuGAP")
  data(observations_sim, package = "imuGAP")
  data(locations_sim, package = "imuGAP")
  # observations_sim has extra columns; the function uses the data.table
  # as-passed, and canonicalize_observations only requires obs_id/positive/
  # sample_n. We pass directly.
  res <- canonicalize_populations(
    data.table::copy(populations_sim),
    data.table::copy(observations_sim),
    data.table::copy(locations_sim),
    max_cohort = max(populations_sim$cohort),
    max_age = max(populations_sim$age)
  )
  expect_s3_class(res, "data.table")
  expect_true("obs_c_id" %in% names(res))
  expect_true("loc_c_id" %in% names(res))
  expect_true("range_start" %in% names(res))
})

test_that("canonicalize_populations coordinates across 1-layer, 2-layer, and 4-layer hierarchies", {
  # 1-layer coordination
  locs1 <- data.frame(loc_id = "state", parent_id = NA)
  locs1_canon <- canonicalize_locations(locs1)
  obs1 <- data.frame(
    obs_id = c("o1", "o2"),
    positive = c(5L, 10L),
    sample_n = c(10L, 20L)
  )
  pops1 <- data.frame(
    obs_id = obs1$obs_id,
    loc_id = rep(locs1$loc_id, nrow(obs1)),
    cohort = c(1L, 1L),
    age = c(5L, 5L),
    dose = c(1L, 2L),
    weight = c(1.0, 1.0)
  )
  res1 <- canonicalize_populations(pops1, obs1, locs1)
  expect_s3_class(res1, "data.table")
  expect_equal(res1$loc_c_id, rep(locs1_canon$loc_c_id, nrow(pops1)))
  expect_equal(res1$obs_c_id, seq_len(nrow(obs1)))

  # 2-layer coordination
  locs2 <- data.frame(
    loc_id = c("state", "cnty1", "cnty2"),
    parent_id = c(NA, "state", "state")
  )
  locs2_canon <- canonicalize_locations(locs2)
  pops2 <- data.frame(
    obs_id = obs1$obs_id,
    loc_id = c("cnty1", "cnty2"),
    cohort = c(1L, 1L),
    age = c(5L, 5L),
    dose = c(1L, 2L),
    weight = c(1.0, 1.0)
  )
  res2 <- canonicalize_populations(pops2, obs1, locs2)
  expect_s3_class(res2, "data.table")
  expect_setequal(
    res2$loc_c_id,
    locs2_canon[loc_id %in% pops2$loc_id, loc_c_id]
  )

  # 4-layer coordination
  locs4 <- data.frame(
    loc_id = c("state", "cnty1", "cnty2", "dist1", "dist2", "schl1", "schl2"),
    parent_id = c(NA, "state", "state", "cnty1", "cnty2", "dist1", "dist1")
  )
  locs4_canon <- canonicalize_locations(locs4)
  pops4 <- data.frame(
    obs_id = obs1$obs_id,
    loc_id = c("schl1", "schl2"),
    cohort = c(1L, 1L),
    age = c(5L, 5L),
    dose = c(1L, 2L),
    weight = c(1.0, 1.0)
  )
  res4 <- canonicalize_populations(pops4, obs1, locs4)
  expect_s3_class(res4, "data.table")
  expect_setequal(
    res4$loc_c_id,
    locs4_canon[loc_id %in% pops4$loc_id, loc_c_id]
  )
})

test_that("canonicalize_populations errors when maximum layer depth has no observations", {
  # 3-layer location tree: state -> cnty1, cnty2 -> schl1, schl2
  locs3 <- make_test_locs()
  obs <- make_test_obs()

  # populations only contains loc_id at layer 2 (cnty1, cnty2)
  bad_pops <- data.frame(
    obs_id = obs$obs_id,
    loc_id = c("cnty1", "cnty2"),
    cohort = c(1L, 1L),
    age = c(2L, 2L),
    dose = c(1L, 2L),
    weight = c(1.0, 1.0)
  )

  expect_error(
    canonicalize_populations(bad_pops, obs, locs3),
    "maximum location layer depth \\(3\\)"
  )

  # 2-layer location tree: state -> cnty1, cnty2, populations only has state (layer 1)
  locs2 <- data.frame(
    loc_id = c("state", "cnty1", "cnty2"),
    parent_id = c(NA, "state", "state")
  )
  bad_pops2 <- data.frame(
    obs_id = obs$obs_id,
    loc_id = c("state", "state"),
    cohort = c(1L, 1L),
    age = c(2L, 2L),
    dose = c(1L, 2L),
    weight = c(1.0, 1.0)
  )

  expect_error(
    canonicalize_populations(bad_pops2, obs, locs2),
    "maximum location layer depth \\(2\\)"
  )
})

test_that("canonicalize_populations succeeds with max layer and higher layer observations", {
  locs3 <- make_test_locs()
  obs3 <- data.frame(
    obs_id = c("o1", "o2", "o3"),
    positive = c(5L, 10L, 15L),
    sample_n = c(10L, 20L, 30L)
  )
  # Mix of county (layer 2) and school (layer 3) observations
  mixed_pops <- data.frame(
    obs_id = obs3$obs_id,
    loc_id = c("cnty1", "schl1", "schl2"),
    cohort = c(1L, 1L, 1L),
    age = c(2L, 2L, 2L),
    dose = c(1L, 2L, 1L),
    weight = c(1.0, 1.0, 1.0)
  )

  res <- canonicalize_populations(mixed_pops, obs3, locs3)
  expect_s3_class(res, "data.table")
  expect_equal(nrow(res), nrow(mixed_pops))
})
