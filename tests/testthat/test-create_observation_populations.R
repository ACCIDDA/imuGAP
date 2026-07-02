test_that("create_observation_populations errors if obs_id is missing", {
  obs <- data.frame(positive = 5, sample_n = 10)
  expect_error(create_observation_populations(obs), "obs_id")
})

test_that("create_observation_populations errors if obs_id is not unique", {
  obs <- make_test_obs()
  obs$obs_id <- c("o1", "o1")
  expect_error(create_observation_populations(obs), "unique")
})

test_that("create_observation_populations errors on missing required columns", {
  obs <- make_test_obs()
  # missing loc_id, cohort, age_min, age_max, dose
  expect_error(
    create_observation_populations(obs, mode = "snapshot"),
    "requires the following column"
  )
})

test_that("create_observation_populations errors on columns present in both observations and ...", {
  obs <- make_test_obs()
  obs$dose <- 1L
  expect_error(
    create_observation_populations(
      obs,
      dose = 1L,
      loc_id = "school",
      cohort = 5L,
      age_min = 1,
      age_max = 2
    ),
    "specified in both"
  )
})

test_that("correctly calculates weights for integer ages", {
  obs <- make_test_obs()[1, ] # "o1"
  res <- create_observation_populations(
    obs,
    mode = "snapshot",
    loc_id = "schl",
    cohort = 5L,
    age_min = 2,
    age_max = 5,
    dose = 1L
  )

  expect_s3_class(res, "data.table")
  expect_equal(nrow(res), 3L)
  expect_equal(res$age, 2:4)
  expect_equal(res$cohort, c(7L, 6L, 5L))
  expect_equal(res$weight, rep(1 / 3, 3L))
  expect_equal(res$obs_id, rep("o1", 3L))
})

test_that("create_observation_populations succeeds and calculates correct weights for real ages", {
  obs <- make_test_obs()[1, ]
  res <- create_observation_populations(
    obs,
    mode = "snapshot",
    loc_id = "schl",
    cohort = 5L,
    age_min = 3.75,
    age_max = 5.25,
    dose = 1L
  )

  expect_equal(res$age, 3:5)
  expect_equal(res$cohort, c(7L, 6L, 5L))
  # age 3 overlap: [3.75, 4.0] -> 0.25
  # age 4 overlap: [4.0, 5.0] -> 1.0
  # age 5 overlap: [5.0, 5.25] -> 0.25
  # span: 1.5
  expect_equal(res$weight, c(0.25 / 1.5, 1.0 / 1.5, 0.25 / 1.5))
})

test_that("create_observation_populations errors on zero span (age_min == age_max)", {
  obs <- make_test_obs()[1, ]

  # real boundary
  expect_error(
    create_observation_populations(
      obs,
      mode = "snapshot",
      loc_id = "schl",
      cohort = 5L,
      age_min = 3.75,
      age_max = 3.75,
      dose = 1L
    ),
    "age_min must be strictly less than age_max"
  )

  # integer boundary
  expect_error(
    create_observation_populations(
      obs,
      mode = "snapshot",
      loc_id = "schl",
      cohort = 5L,
      age_min = 3.0,
      age_max = 3.0,
      dose = 1L
    ),
    "age_min must be strictly less than age_max"
  )
})

test_that("create_observation_populations succeeds when age_max is omitted", {
  obs <- make_test_obs()[1, ]

  res <- create_observation_populations(
    obs,
    mode = "snapshot",
    loc_id = "schl",
    cohort = 5L,
    age_min = 3.0,
    dose = 1L
  )

  expect_s3_class(res, "data.table")
  expect_equal(nrow(res), 1L)
  expect_equal(res$age, 3L)
  expect_equal(res$cohort, 5L)
  expect_equal(res$weight, 1.0)
})

test_that("create_observation_populations validates cohort, dose, age_min, age_max", {
  obs <- make_test_obs()[1, ]

  # invalid dose
  expect_error(
    create_observation_populations(
      obs,
      loc_id = "schl",
      cohort = 5L,
      age_min = 1,
      age_max = 2,
      dose = -1L
    ),
    "dose"
  )

  # invalid cohort
  expect_error(
    create_observation_populations(
      obs,
      loc_id = "schl",
      cohort = 0L,
      age_min = 1,
      age_max = 2,
      dose = 1L
    ),
    "cohort"
  )

  # invalid age_min (not numeric)
  expect_error(
    create_observation_populations(
      obs,
      loc_id = "schl",
      cohort = 5L,
      age_min = "one",
      age_max = 2,
      dose = 1L
    ),
    "age_min"
  )

  # invalid age_min <= 0
  expect_error(
    create_observation_populations(
      obs,
      loc_id = "schl",
      cohort = 5L,
      age_min = 0,
      age_max = 2,
      dose = 1L
    ),
    "age_min"
  )

  # age_min greater than age_max
  expect_error(
    create_observation_populations(
      obs,
      loc_id = "schl",
      cohort = 5L,
      age_min = 3,
      age_max = 2,
      dose = 1L
    ),
    "age_min.*age_max"
  )
})

test_that("output of create_observation_populations can be canonicalized", {
  obs <- make_test_obs()
  pops <- create_observation_populations(
    obs,
    mode = "snapshot",
    loc_id = "schl",
    cohort = 5L,
    age_min = 2,
    age_max = 5,
    dose = 1L
  )

  expect_no_error(
    canonicalize_populations(
      pops,
      obs,
      make_test_locs(),
      max_cohort = 10L,
      max_age = 10L
    )
  )

  # Inappropriate loc_id runs without error, but fails canonicalization
  expect_no_error(
    pops_bad <- create_observation_populations(
      obs,
      mode = "snapshot",
      loc_id = "nonsense",
      cohort = 5L,
      age_min = 2,
      age_max = 5,
      dose = 1L
    )
  )

  expect_error(
    canonicalize_populations(
      pops_bad,
      obs,
      make_test_locs(),
      max_cohort = 10L,
      max_age = 10L
    ),
    "loc_id.*parent set"
  )
})
