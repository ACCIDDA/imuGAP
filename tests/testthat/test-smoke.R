# Smoke tests using simulated test data from issue #14
#
# Data: school_observations.csv, state_observations.csv (Claire, issue #14)
# Derived: locations.csv (state > county > school hierarchy from school data)
#
# NOTE: obs_populations.csv is not yet available. Once provided, add a full
# pipeline test calling imuGAP() with minimal Stan iterations.

# --- load package R sources (no Stan/Rcpp needed for validation) --------------

pkg_root <- file.path("..", "..")
for (src in c("R/checkers.R", "R/imuGAP.R")) {
  source(file.path(pkg_root, src), local = TRUE)
}

# --- check_observations -------------------------------------------------------

test_that("school observations pass check_observations()", {
  path <- test_path("testdata", "school_observations.csv")
  skip_if_not(file.exists(path), "school_observations.csv not available")

  obs <- read.csv(path)
  names(obs)[names(obs) == "y_obs"] <- "positive"
  names(obs)[names(obs) == "y_smp"] <- "sample_n"

  result <- check_observations(obs)
  expect_s3_class(result, "data.table")
  expect_true("id" %in% names(result))
  expect_equal(nrow(result), 600L)
  expect_true(all(result$positive <= result$sample_n))
})

test_that("state observations pass check_observations()", {
  path <- test_path("testdata", "state_observations.csv")
  skip_if_not(file.exists(path), "state_observations.csv not available")

  obs <- read.csv(path)
  names(obs)[names(obs) == "y_obs"] <- "positive"
  names(obs)[names(obs) == "y_smp"] <- "sample_n"

  result <- check_observations(obs)
  expect_s3_class(result, "data.table")
  expect_true("id" %in% names(result))
  expect_equal(nrow(result), 98L)
  expect_true(all(result$positive <= result$sample_n))
})

# --- check_locations ----------------------------------------------------------

test_that("derived locations pass check_locations()", {
  path <- test_path("testdata", "locations.csv")
  skip_if_not(file.exists(path), "locations.csv not available")

  # globalenv assignment works around NSE frame-walking in check_locations()
  # (same pattern as inst/scripts/imugap.R)
  locs <- read.csv(path)
  assign("locs", locs, envir = globalenv())
  on.exit(rm("locs", envir = globalenv()), add = TRUE)

  result <- check_locations(locs)
  expect_s3_class(result, "data.table")
  expect_true("layer" %in% names(result))
  expect_equal(nrow(result), 28L)
  expect_equal(max(result$layer), 3L)
  expect_equal(result[layer == 1, .N], 1L)
  expect_equal(result[layer == 2, .N], 3L)
  expect_equal(result[layer == 3, .N], 24L)
})

# --- cross-file consistency ---------------------------------------------------

test_that("school unit_ids align with location hierarchy", {
  school_path <- test_path("testdata", "school_observations.csv")
  loc_path <- test_path("testdata", "locations.csv")
  skip_if_not(
    file.exists(school_path) && file.exists(loc_path),
    "test data not available"
  )

  obs <- read.csv(school_path)
  locs <- read.csv(loc_path)

  school_ids <- unique(obs$unit_id)
  expect_true(all(school_ids %in% locs$id))

  county_map <- unique(obs[, c("enc_unit_id", "county")])
  for (i in seq_len(nrow(county_map))) {
    enc <- county_map$enc_unit_id[i]
    county_loc_id <- enc + 1L
    expect_true(county_loc_id %in% locs$id)
    expect_equal(locs$parent_id[locs$id == county_loc_id], 1L)
  }
})

# --- full pipeline (blocked on obs_populations) -------------------------------

test_that("full imuGAP() pipeline smoke test", {
  skip("obs_populations.csv not yet available (issue #14)")

  obs <- read.csv(test_path("testdata", "school_observations.csv"))
  names(obs)[names(obs) == "y_obs"] <- "positive"
  names(obs)[names(obs) == "y_smp"] <- "sample_n"

  locs <- read.csv(test_path("testdata", "locations.csv"))
  opop <- read.csv(test_path("testdata", "obs_populations.csv"))

  expect_no_error(
    imuGAP(
      obs, opop, locs,
      dose_schedule = c(1L, 4L),
      stan_opts = stan_options(iter = 10, chains = 1)
    )
  )
})
