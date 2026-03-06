# Tests for inst/scripts/imugap.R
#
# NOTE: main() integration with imuGAP::check_* and imuGAP::imuGAP is not
# tested here -- only argument parsing, I/O error paths, and helper functions.
# Full integration testing requires a subprocess with real input data.

# --- Source helpers and main() from the CLI script ---------------------------

# Prefer local dev copy (assumes tests run from tests/testthat/).
# Falls back to installed package version if relative path doesn't resolve.
script_path <- file.path("../../inst/scripts/imugap.R")
if (!file.exists(script_path)) {
  script_path <- system.file("scripts", "imugap.R", package = "imuGAP")
}
if (!nzchar(script_path) || !file.exists(script_path)) {
  stop("Cannot find imugap.R: tried relative path and installed package location")
}

script_lines <- readLines(script_path)
fn_start <- grep("^USAGE", script_lines)[1]
main_end <- grep("^if \\(!interactive\\(\\)\\)", script_lines)[1] - 1
stopifnot(
  "Could not find USAGE marker in script" = !is.na(fn_start),
  "Could not find entry guard marker in script" = !is.na(main_end),
  "Script markers are in wrong order" = fn_start < main_end
)

# Extract everything from USAGE through main(), skipping the package guard block
guard_start <- grep("^if \\(!requireNamespace", script_lines)[1]
guard_end <- guard_start + 2L  # 3-line block: if (...) { / stop(...) / }
keep <- setdiff(fn_start:main_end, guard_start:guard_end)
source_text <- script_lines[keep]

env <- new.env(parent = globalenv())
tmp_src <- tempfile(fileext = ".R")
writeLines(source_text, tmp_src)
sys.source(tmp_src, envir = env)
unlink(tmp_src)
attach(env, name = "imugap_cli")
on.exit(detach("imugap_cli"), add = TRUE)

# --- find_input_file ---------------------------------------------------------

test_that("find_input_file reads CSV from directory", {
  dir <- tempfile("test_csv_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  write.csv(data.frame(positive = 1:3, sample_n = 10:12),
            file.path(dir, "observations.csv"), row.names = FALSE)
  result <- find_input_file(dir, "observations")
  expect_equal(result$positive, 1:3)
  expect_equal(result$sample_n, 10:12)
})

test_that("find_input_file reads RDS from directory", {
  dir <- tempfile("test_rds_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  expected <- data.frame(id = 1:5, parent_id = c(NA, 1, 1, 2, 2))
  saveRDS(expected, file.path(dir, "locations.rds"))
  result <- find_input_file(dir, "locations")
  expect_equal(result, expected)
})

test_that("find_input_file errors with clear message when file missing", {
  dir <- tempfile("test_missing_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  expect_error(
    find_input_file(dir, "nonexistent"),
    "Expected 'nonexistent\\.csv' or 'nonexistent\\.rds'"
  )
})

# --- check_all_inputs --------------------------------------------------------

test_that("check_all_inputs reports all missing files at once", {
  dir <- tempfile("test_all_missing_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  expect_error(
    check_all_inputs(dir),
    "observations.*obs_populations.*locations"
  )
})

test_that("check_all_inputs reports only the actually missing files", {
  dir <- tempfile("test_partial_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  write.csv(data.frame(a = 1), file.path(dir, "observations.csv"), row.names = FALSE)
  err <- tryCatch(check_all_inputs(dir), error = identity)
  expect_true(inherits(err, "error"))
  expect_false(grepl("observations", err$message))
  expect_true(grepl("obs_populations", err$message))
  expect_true(grepl("locations", err$message))
})

test_that("check_all_inputs passes when all files present", {
  dir <- tempfile("test_all_present_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  write.csv(data.frame(a = 1), file.path(dir, "observations.csv"), row.names = FALSE)
  write.csv(data.frame(a = 1), file.path(dir, "obs_populations.csv"), row.names = FALSE)
  write.csv(data.frame(a = 1), file.path(dir, "locations.csv"), row.names = FALSE)
  expect_no_error(check_all_inputs(dir))
})

# --- load_by_ext -------------------------------------------------------------

test_that("load_by_ext reads CSV correctly", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  write.csv(data.frame(x = 1:3), path, row.names = FALSE)
  result <- load_by_ext(path)
  expect_equal(result$x, 1:3)
})

test_that("load_by_ext reads RDS correctly", {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  expected <- data.frame(x = 1:3)
  saveRDS(expected, path)
  expect_equal(load_by_ext(path), expected)
})

test_that("load_by_ext rejects unsupported extensions", {
  path <- tempfile(fileext = ".json")
  writeLines("{}", path)
  on.exit(unlink(path))
  expect_error(load_by_ext(path), "Unsupported extension")
})

test_that("load_by_ext includes filename in error for corrupt files", {
  path <- tempfile(fileext = ".rds")
  writeLines("not a valid rds file", path)
  on.exit(unlink(path))
  expect_error(load_by_ext(path), "Failed to read")
})

# --- main() argument parsing and error paths ---------------------------------

test_that("main returns 0 and prints usage for no args", {
  out <- capture.output(result <- main(character(0)))
  expect_equal(result, 0)
  expect_true(any(grepl("imugap", out)))
})

test_that("main returns 0 and prints usage for --help", {
  out <- capture.output(result <- main(c("--help")))
  expect_equal(result, 0)
  expect_true(any(grepl("imugap", out)))
})

test_that("main returns 0 and prints usage for -h alone", {
  out <- capture.output(result <- main(c("-h")))
  expect_equal(result, 0)
})

test_that("main returns 3 for non-existent input directory", {
  result <- suppressMessages(main(c("/nonexistent/path/xyz")))
  expect_equal(result, 3)
})

test_that("main returns 3 when input files are missing", {
  dir <- tempfile("test_main_missing_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  result <- suppressMessages(main(c(dir)))
  expect_equal(result, 3)
})
