# Tests for inst/scripts/imugap.R

# Prefer local dev copy over installed package version
script_path <- file.path("../../inst/scripts/imugap.R")
if (!file.exists(script_path)) {
  script_path <- system.file("scripts", "imugap.R", package = "imuGAP")
}

script_lines <- readLines(script_path)
fn_start <- grep("^SUPPORTED_EXT", script_lines)[1]
main_end <- grep("^if \\(!interactive\\(\\)\\)", script_lines)[1] - 1
source_text <- script_lines[fn_start:main_end]
# Remove quit() calls so tests don't exit R
source_text <- gsub("quit\\(.*?\\)", "invisible(NULL)", source_text)
parse(text = source_text) # verify it parses
env <- new.env(parent = globalenv())
# Write filtered source to a temp file for sys.source
tmp_src <- tempfile(fileext = ".R")
writeLines(source_text, tmp_src)
sys.source(tmp_src, envir = env)
unlink(tmp_src)
attach(env, name = "imugap_cli")
on.exit(detach("imugap_cli"), add = TRUE)

# --- find_input_file ---------------------------------------------------------

test_that("find_input_file reads CSV from directory", {
  dir <- tempdir()
  csv_path <- file.path(dir, "observations.csv")
  write.csv(data.frame(positive = 1:3, sample_n = 10:12), csv_path, row.names = FALSE)
  on.exit(unlink(csv_path), add = TRUE)

  result <- find_input_file(dir, "observations")
  expect_equal(nrow(result), 3)
  expect_true("positive" %in% names(result))
})

test_that("find_input_file reads RDS from directory", {
  dir <- tempdir()
  rds_path <- file.path(dir, "locations.rds")
  saveRDS(data.frame(id = 1:5, parent_id = c(NA, 1, 1, 2, 2)), rds_path)
  on.exit(unlink(rds_path), add = TRUE)

  result <- find_input_file(dir, "locations")
  expect_equal(nrow(result), 5)
})

test_that("find_input_file errors with clear message when file missing", {
  dir <- tempdir()
  expect_error(
    find_input_file(dir, "nonexistent"),
    "Expected 'nonexistent\\.csv' or 'nonexistent\\.rds'"
  )
})

# --- check_all_inputs --------------------------------------------------------

test_that("check_all_inputs reports all missing files at once", {
  dir <- tempdir()
  # Clean up any leftover files from other tests
  unlink(file.path(dir, "observations.csv"))
  unlink(file.path(dir, "obs_populations.csv"))
  unlink(file.path(dir, "locations.csv"))

  expect_error(
    check_all_inputs(dir),
    "observations.*obs_populations.*locations"
  )
})

test_that("check_all_inputs passes when all files present", {
  dir <- tempdir()
  write.csv(data.frame(a = 1), file.path(dir, "observations.csv"), row.names = FALSE)
  write.csv(data.frame(a = 1), file.path(dir, "obs_populations.csv"), row.names = FALSE)
  write.csv(data.frame(a = 1), file.path(dir, "locations.csv"), row.names = FALSE)
  on.exit({
    unlink(file.path(dir, "observations.csv"))
    unlink(file.path(dir, "obs_populations.csv"))
    unlink(file.path(dir, "locations.csv"))
  }, add = TRUE)

  expect_no_error(check_all_inputs(dir))
})

# --- load_by_ext -------------------------------------------------------------

test_that("load_by_ext rejects unsupported extensions", {
  path <- tempfile(fileext = ".json")
  writeLines("{}", path)
  on.exit(unlink(path))
  expect_error(load_by_ext(path), "Unsupported extension")
})
