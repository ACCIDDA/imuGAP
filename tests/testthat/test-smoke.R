# Smoke tests using simulated test data from issue #14
#
# Data sources:
#   - CSV fixtures: school_observations.csv, state_observations.csv (issue #14)
#   - Package data: observations_sim, obs_populations_sim, locations_sim (.rda)
#   - Derived: locations.csv (from school CSV hierarchy)

# --- load package R sources (no Stan/Rcpp needed for validation) --------------

pkg_root <- file.path("..", "..")
for (src in c("R/checkers.R", "R/imuGAP.R")) {
  source(file.path(pkg_root, src), local = TRUE)
}

# --- helper: load .rda into local env ----------------------------------------

load_rda <- function(name) {
  path <- file.path(pkg_root, "data", paste0(name, ".rda"))
  if (!file.exists(path)) return(NULL)
  env <- new.env(parent = emptyenv())
  load(path, envir = env)
  env[[name]]
}

# --- CSV: check_observations --------------------------------------------------

test_that("school observations CSV passes check_observations()", {
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

test_that("state observations CSV passes check_observations()", {
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

# --- CSV: check_locations -----------------------------------------------------

test_that("derived locations CSV passes check_locations()", {
  path <- test_path("testdata", "locations.csv")
  skip_if_not(file.exists(path), "locations.csv not available")

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

# --- CSV: cross-file consistency ----------------------------------------------

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

# --- package data: full validation chain --------------------------------------

test_that("observations_sim passes check_observations()", {
  obs <- load_rda("observations_sim")
  skip_if(is.null(obs), "observations_sim.rda not available")

  names(obs)[names(obs) == "y_obs"] <- "positive"
  names(obs)[names(obs) == "y_smp"] <- "sample_n"

  result <- check_observations(obs)
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 698L)
  expect_true(all(result$positive <= result$sample_n))
})

test_that("locations_sim passes check_locations()", {
  locs <- load_rda("locations_sim")
  skip_if(is.null(locs), "locations_sim.rda not available")

  assign("locs", locs, envir = globalenv())
  on.exit(rm("locs", envir = globalenv()), add = TRUE)

  result <- check_locations(locs)
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 28L)
  expect_equal(max(result$layer), 3L)
})

test_that("obs_populations_sim passes check_obs_population()", {
  obs_raw <- load_rda("observations_sim")
  opop <- load_rda("obs_populations_sim")
  locs_raw <- load_rda("locations_sim")
  skip_if(
    is.null(obs_raw) || is.null(opop) || is.null(locs_raw),
    "package data not available"
  )

  names(obs_raw)[names(obs_raw) == "y_obs"] <- "positive"
  names(obs_raw)[names(obs_raw) == "y_smp"] <- "sample_n"
  obs <- check_observations(obs_raw)

  assign("locs_raw", locs_raw, envir = globalenv())
  on.exit(rm("locs_raw", envir = globalenv()), add = TRUE)
  loc_info <- check_locations(locs_raw)

  result <- check_obs_population(opop, obs, loc_info)
  expect_s3_class(result, "data.table")
  expect_equal(length(unique(result$obs_id)), nrow(obs))
  expect_true(all(result$dose %in% 1:2))
})

# --- full pipeline (requires Stan) -------------------------------------------

test_that("full imuGAP() pipeline smoke test", {
  skip("requires Stan compilation — run manually with full package install")

  obs_raw <- load_rda("observations_sim")
  opop <- load_rda("obs_populations_sim")
  locs_raw <- load_rda("locations_sim")

  names(obs_raw)[names(obs_raw) == "y_obs"] <- "positive"
  names(obs_raw)[names(obs_raw) == "y_smp"] <- "sample_n"

  assign("locs_raw", locs_raw, envir = globalenv())
  on.exit(rm("locs_raw", envir = globalenv()), add = TRUE)

  expect_no_error(
    imuGAP(
      obs_raw, opop, locs_raw,
      dose_schedule = c(1L, 4L),
      stan_opts = stan_options(iter = 10, chains = 1)
    )
  )
})
