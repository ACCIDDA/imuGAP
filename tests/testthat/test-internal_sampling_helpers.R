# Unit tests for internal data preparation helpers in R/imuGAP.R
# (build_interval_schedule and slice_weights)

library(data.table)

# ==============================================================================
# 1. build_interval_schedule Tests
# ==============================================================================

test_that("build_interval_schedule constructs valid interval schedules for standard inputs", {
  dose_sched <- c(1L, 4L, 11L)
  ages <- c(4L, 5L, 12L, 18L)
  max_age <- max(ages)

  res <- build_interval_schedule(dose_sched, ages)

  valid_sched <- dose_sched[dose_sched >= 1L & dose_sched <= max_age]
  valid_ages <- ages[ages >= 1L & ages <= max_age]
  expected_eval_ages <- sort(unique(c(valid_sched, valid_ages, max_age)))
  expected_dt <- as.numeric(diff(c(0L, expected_eval_ages)))
  expected_sched_mat <- outer(expected_eval_ages, dose_sched, ">") * 1.0

  expect_type(res, "list")
  expect_named(
    res,
    c("n_intervals", "dt_vec", "dose_sched", "age_to_interval_map")
  )

  # Check lengths and dynamic diff properties
  expect_equal(res$n_intervals, length(expected_eval_ages))
  expect_equal(res$dt_vec, expected_dt)
  expect_equal(sum(res$dt_vec), max_age)
  expect_true(all(res$dt_vec > 0))

  # Check dimensions and contents of dose schedule matrix
  expect_equal(nrow(res$dose_sched), length(expected_eval_ages))
  expect_equal(ncol(res$dose_sched), length(dose_sched))
  expect_equal(res$dose_sched, expected_sched_mat)

  # Check age to interval mapping
  expect_length(res$age_to_interval_map, max_age)
  expect_true(all(
    res$age_to_interval_map >= 1L & res$age_to_interval_map <= res$n_intervals
  ))
})

test_that("build_interval_schedule handles sparse observed ages and interpolates unobserved ages", {
  dose_sched <- c(2L, 7L)
  ages <- c(7L, 15L)
  max_age <- max(ages)

  res <- build_interval_schedule(dose_sched, ages)

  valid_sched <- dose_sched[dose_sched >= 1L & dose_sched <= max_age]
  valid_ages <- ages[ages >= 1L & ages <= max_age]
  expected_eval_ages <- sort(unique(c(valid_sched, valid_ages, max_age)))
  expected_t_points <- c(0L, expected_eval_ages)
  expected_dt <- as.numeric(diff(expected_t_points))
  expected_sched_mat <- outer(expected_eval_ages, dose_sched, ">") * 1.0
  expected_age_map <- findInterval(
    seq_len(max_age),
    expected_t_points,
    left.open = TRUE
  )

  expect_equal(res$n_intervals, length(expected_eval_ages))
  expect_equal(res$dt_vec, expected_dt)
  expect_equal(res$dose_sched, expected_sched_mat)
  expect_equal(res$age_to_interval_map, expected_age_map)
})

test_that("build_interval_schedule rejects empty or invalid ages", {
  # NULL ages
  expect_error(
    build_interval_schedule(c(1L, 4L), ages = NULL),
    err_pattern(ERR_AGES_EMPTY)
  )

  # Empty ages vector
  expect_error(
    build_interval_schedule(c(1L, 4L), ages = integer(0)),
    err_pattern(ERR_AGES_EMPTY)
  )

  # All NA ages
  expect_error(
    build_interval_schedule(c(1L, 4L), ages = c(NA_integer_, NA_integer_)),
    err_pattern(ERR_AGES_EMPTY)
  )
})

test_that("build_interval_schedule rejects empty or out-of-bound dose schedules", {
  # Empty dose schedule
  expect_error(
    build_interval_schedule(integer(0), ages = c(1L, 2L, 5L)),
    err_pattern(ERR_DOSE_SCHEDULE_EMPTY)
  )

  # Entirely out of bounds dose schedule relative to max(ages)
  dose_sched_out <- c(99L, 100L)
  expect_error(
    build_interval_schedule(
      dose_sched_out,
      ages = c(1L, 2L, 5L)
    ),
    err_pattern(ERR_DOSE_SCHED_OOB, max_age = 5L)
  )
})

test_that("build_interval_schedule handles minimal valid age 1 boundary", {
  single_age <- 1L
  res_single <- build_interval_schedule(
    single_age,
    single_age
  )
  expect_equal(res_single$n_intervals, length(single_age))
  expect_equal(res_single$dt_vec, as.numeric(single_age))
  expect_equal(res_single$age_to_interval_map, single_age)
})

# ==============================================================================
# 1b. validate_dose_schedule Tests
# ==============================================================================

test_that("validate_dose_schedule accepts valid population metadata", {
  wts <- data.table(
    obs_id = c("obs1", "obs2", "obs3", "obs3"),
    dose = c(1L, 2L, 2L, 2L),
    age = c(2L, 5L, 4L, 6L)
  )
  expect_invisible(validate_dose_schedule(c(1L, 4L), wts))
})

test_that("validate_dose_schedule rejects doses exceeding schedule length", {
  wts <- data.table(
    obs_id = c("obs1", "obs2"),
    dose = c(1L, 3L),
    age = c(2L, 10L)
  )
  expect_error(
    validate_dose_schedule(c(1L, 4L), wts),
    err_pattern(ERR_POP_DOSE_EXCEEDS_SCHED, n_doses = 2L, max_dose = 3L)
  )
})

test_that("validate_dose_schedule rejects when final dose is not observed in populations", {
  wts <- data.table(
    obs_id = c("obs1", "obs2"),
    dose = c(1L, 1L),
    age = c(2L, 5L)
  )
  # Final dose 2 is not observed in wts
  expect_error(
    validate_dose_schedule(c(1L, 4L), wts),
    err_pattern(ERR_DOSE_FINAL_NOT_OBSERVED, n_doses = 2L)
  )
})

test_that("validate_dose_schedule rejects observations younger than schedule", {
  # Unmixed observation with age <= dose changepoint (obs2 age 4 <= dose 2 changepoint 4)
  wts_unmixed <- data.table(
    obs_id = c("obs1", "obs2", "obs3"),
    dose = c(1L, 2L, 1L),
    age = c(2L, 4L, 10L)
  )
  expect_error(
    validate_dose_schedule(c(1L, 4L), wts_unmixed),
    err_pattern(ERR_POP_DOSE_INCOMPATIBLE, dose = 2L, sched_age = 4L)
  )

  # Mixed observation where ALL contributing ages are <= dose changepoint
  wts_mixed_bad <- data.table(
    obs_id = c("obs1", "obs2", "obs2", "obs3"),
    dose = c(1L, 2L, 2L, 1L),
    age = c(2L, 3L, 4L, 10L)
  )
  expect_error(
    validate_dose_schedule(c(1L, 4L), wts_mixed_bad),
    err_pattern(ERR_POP_DOSE_INCOMPATIBLE, dose = 2L, sched_age = 4L)
  )

  # Mixed observation where at least ONE age is > dose changepoint (valid)
  wts_mixed_good <- data.table(
    obs_id = c("obs1", "obs2", "obs2"),
    dose = c(1L, 2L, 2L),
    age = c(2L, 4L, 5L)
  )
  expect_invisible(validate_dose_schedule(
    c(1L, 4L),
    wts_mixed_good
  ))
})

# ==============================================================================
# 2. slice_weights Tests (Canonical Data Inputs)
# ==============================================================================

test_that("slice_weights handles empty canonical observation slices", {
  can_locs <- canonicalize_locations(make_test_locs())
  can_obs <- canonicalize_observations(make_test_obs())
  can_wts <- canonicalize_populations(
    make_test_pops(),
    observations = can_obs,
    locations = can_locs
  )

  can_obs_empty <- can_obs[0]
  res <- slice_weights(can_wts, can_obs_empty, "uncensored")

  expect_type(res, "list")
  expect_equal(res$n_obs_unmixed_uncensored, 0L)
  expect_equal(res$n_obs_mixed_uncensored, 0L)
  expect_equal(res$n_weights_mixed_uncensored, 0L)
  expect_length(res$y_obs_unmixed_uncensored, 0L)
  expect_length(res$y_smp_unmixed_uncensored, 0L)
  expect_length(res$y_obs_mixed_uncensored, 0L)
  expect_length(res$y_smp_mixed_uncensored, 0L)
  expect_length(res$w_cohort_unmixed_uncensored, 0L)
  expect_length(res$weights_mixed_uncensored, 0L)
})

test_that("slice_weights partitions pure unmixed canonical observations accurately", {
  can_locs <- canonicalize_locations(make_test_locs())
  raw_obs <- data.frame(
    obs_id = c("o1", "o2", "o3"),
    positive = c(8L, 15L, 22L),
    sample_n = c(10L, 20L, 25L)
  )
  can_obs <- canonicalize_observations(raw_obs)
  raw_pops <- data.frame(
    obs_id = c("o1", "o2", "o3"),
    loc_id = c("schl1", "schl2", "cnty2"),
    cohort = c(1L, 2L, 3L),
    age = c(5L, 6L, 7L),
    dose = c(1L, 2L, 1L),
    weight = c(1.0, 1.0, 1.0)
  )
  can_wts <- canonicalize_populations(
    raw_pops,
    observations = can_obs,
    locations = can_locs
  )

  expect_true(is_canonical(can_obs, "observations"))
  expect_true(is_canonical(can_wts, "populations"))

  res <- slice_weights(can_wts, can_obs, "uncensored")

  order_map <- match(can_obs$obs_c_id, can_wts$obs_c_id)

  expect_equal(res$n_obs_unmixed_uncensored, nrow(can_obs))
  expect_equal(res$n_obs_mixed_uncensored, 0L)
  expect_equal(res$n_weights_mixed_uncensored, 0L)

  expect_equal(res$y_obs_unmixed_uncensored, can_obs$positive)
  expect_equal(res$y_smp_unmixed_uncensored, can_obs$sample_n)
  expect_equal(res$w_loc_unmixed_uncensored, can_wts$loc_c_id[order_map])
  expect_equal(res$w_cohort_unmixed_uncensored, can_wts$cohort[order_map])
  expect_equal(res$w_age_unmixed_uncensored, can_wts$age[order_map])
  expect_equal(res$w_dose_unmixed_uncensored, can_wts$dose[order_map])
})

test_that("slice_weights partitions pure mixed canonical observations with correct bounds", {
  can_locs <- canonicalize_locations(make_test_locs())
  raw_obs <- data.frame(
    obs_id = c("m1", "m2"),
    positive = c(30L, 40L),
    sample_n = c(50L, 60L),
    censored = c(1, 1)
  )
  can_obs <- canonicalize_observations(raw_obs)
  raw_pops <- data.frame(
    obs_id = c("m1", "m1", "m2", "m2", "m2"),
    loc_id = c("schl1", "schl2", "schl1", "schl2", "cnty2"),
    cohort = c(1L, 1L, 2L, 2L, 2L),
    age = c(5L, 5L, 6L, 6L, 6L),
    dose = c(1L, 1L, 2L, 2L, 2L),
    weight = c(0.4, 0.6, 0.2, 0.3, 0.5)
  )
  can_wts <- canonicalize_populations(
    raw_pops,
    observations = can_obs,
    locations = can_locs
  )

  expect_true(is_canonical(can_obs, "observations"))
  expect_true(is_canonical(can_wts, "populations"))

  res <- slice_weights(can_wts, can_obs, "right")

  w_counts <- can_wts[, .N, by = obs_c_id]
  expected_bounds <- cumsum(c(1L, head(w_counts$N, -1L)))

  expect_equal(res$n_obs_unmixed_right, 0L)
  expect_equal(res$n_obs_mixed_right, nrow(can_obs))
  expect_equal(res$y_obs_mixed_right, can_obs$positive)
  expect_equal(res$y_smp_mixed_right, can_obs$sample_n)
  expect_equal(res$n_weights_mixed_right, nrow(can_wts))
  expect_equal(res$obs_bounds_mixed_right, expected_bounds)
  expect_equal(res$weights_mixed_right, can_wts$weight)
})

test_that("slice_weights partitions combined unmixed and mixed canonical observations correctly", {
  can_locs <- canonicalize_locations(make_test_locs())
  raw_obs <- data.frame(
    obs_id = c("u1", "m1", "u2"),
    positive = c(5L, 12L, 18L),
    sample_n = c(10L, 15L, 20L)
  )
  can_obs <- canonicalize_observations(raw_obs)
  raw_pops <- data.frame(
    obs_id = c("u1", "m1", "m1", "u2"),
    loc_id = c("schl1", "schl1", "schl2", "cnty2"),
    cohort = c(1L, 2L, 2L, 3L),
    age = c(4L, 5L, 5L, 6L),
    dose = c(1L, 1L, 1L, 2L),
    weight = c(1.0, 0.5, 0.5, 1.0)
  )
  can_wts <- canonicalize_populations(
    raw_pops,
    observations = can_obs,
    locations = can_locs
  )

  expect_true(is_canonical(can_obs, "observations"))
  expect_true(is_canonical(can_wts, "populations"))

  res <- slice_weights(can_wts, can_obs, "left")

  w_counts <- can_wts[, .(n_w = .N), by = obs_c_id]
  expected_unmixed_ids <- w_counts[n_w == 1L, obs_c_id]
  expected_mixed_ids <- w_counts[n_w > 1L, obs_c_id]

  expected_unmixed_idx <- which(can_obs$obs_c_id %in% expected_unmixed_ids)
  expected_mixed_idx <- which(can_obs$obs_c_id %in% expected_mixed_ids)

  unmixed_map <- match(can_obs$obs_c_id[expected_unmixed_idx], can_wts$obs_c_id)
  w_mixed <- can_wts[obs_c_id %in% expected_mixed_ids]

  expect_equal(res$n_obs_unmixed_left, length(expected_unmixed_idx))
  expect_equal(res$y_obs_unmixed_left, can_obs$positive[expected_unmixed_idx])
  expect_equal(res$y_smp_unmixed_left, can_obs$sample_n[expected_unmixed_idx])
  expect_equal(res$w_loc_unmixed_left, can_wts$loc_c_id[unmixed_map])
  expect_equal(res$w_cohort_unmixed_left, can_wts$cohort[unmixed_map])

  expect_equal(res$n_obs_mixed_left, length(expected_mixed_idx))
  expect_equal(res$y_obs_mixed_left, can_obs$positive[expected_mixed_idx])
  expect_equal(res$y_smp_mixed_left, can_obs$sample_n[expected_mixed_idx])
  expect_equal(res$n_weights_mixed_left, nrow(w_mixed))
  expect_equal(res$weights_mixed_left, w_mixed$weight)
})
