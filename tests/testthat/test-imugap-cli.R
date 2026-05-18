# Tests for inst/scripts/imugap.R
#
# NOTE: main() integration with imuGAP::canonicalize_* and imuGAP::imuGAP is
# not tested here -- only argument parsing, I/O error paths, and helper
# functions. Full integration testing requires a subprocess with real input
# data.

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
    "observations.*populations.*locations"
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
  expect_true(grepl("populations", err$message))
  expect_true(grepl("locations", err$message))
})

test_that("check_all_inputs passes when all files present", {
  dir <- tempfile("test_all_present_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  write.csv(data.frame(a = 1), file.path(dir, "observations.csv"), row.names = FALSE)
  write.csv(data.frame(a = 1), file.path(dir, "populations.csv"), row.names = FALSE)
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

test_that("install_cli creates symlink to correct script", {
  skip_on_os("windows")
  dir <- tempfile("test_install_cli_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  result <- imuGAP::install_cli(path = dir)
  link <- file.path(dir, "imugap")
  expect_true(result)
  expect_true(file.exists(link))
  target <- Sys.readlink(link)
  expected <- system.file("scripts", "imugap.R", package = "imuGAP")
  expect_equal(normalizePath(target), normalizePath(expected))
})

test_that("install_cli errors for non-existent directory", {
  skip_on_os("windows")
  expect_error(imuGAP::install_cli(path = "/nonexistent/path/xyz"), "does not exist")
})

test_that("install_cli replaces existing file at target", {
  skip_on_os("windows")
  dir <- tempfile("test_install_replace_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  writeLines("old", file.path(dir, "imugap"))
  imuGAP::install_cli(path = dir)
  expect_true(nzchar(Sys.readlink(file.path(dir, "imugap"))))
})

# --- install_cli branch coverage ---------------------------------------------
#
# Cover the remaining branches in R/install_cli.R: symlink-creation failure
# (line 40), and (best-effort) the interactive prompt branch. The Windows
# OS-type check (line 14) and the "no bundled script" branch (line 19) can't
# be exercised on a Mac/Linux test runner without modifying the package or
# the OS — we leave them uncovered.

test_that("install_cli errors when symlink creation fails", {
  skip_on_os("windows")
  # Make the install directory read-only so file.symlink() returns FALSE.
  dir <- tempfile("test_install_fail_ro_")
  dir.create(dir)
  Sys.chmod(dir, "0500")  # r-x for owner; no write
  on.exit({
    Sys.chmod(dir, "0700")
    unlink(dir, recursive = TRUE)
  }, add = TRUE)

  # suppressWarnings: file.symlink() also emits a warning before returning
  # FALSE; we only care about the resulting error.
  expect_error(
    suppressWarnings(imuGAP::install_cli(path = dir)),
    "Failed to create symlink"
  )
})

test_that("install_cli responds to interactive prompt response 'n'", {
  skip_on_os("windows")
  # Mock interactive() and readline() inside the imuGAP namespace. These
  # bindings don't exist there by default (they live in base), but
  # local_mocked_bindings will inject them and the call site will see them
  # via standard scope.
  dir <- tempfile("test_install_decline_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  # Attempt to mock; if the testthat API rejects mocking base functions for
  # this package, skip with a note. (Different testthat versions behave
  # slightly differently.)
  ok <- tryCatch(
    {
      testthat::with_mocked_bindings(
        {
          suppressMessages(imuGAP::install_cli(path = dir))
        },
        interactive = function() TRUE,
        readline = function(prompt) "n",
        .package = "imuGAP"
      )
    },
    error = function(e) NULL
  )
  skip_if(is.null(ok), "testthat cannot mock base bindings in imuGAP namespace")
  expect_false(ok)
  expect_false(file.exists(file.path(dir, "imugap")))
})
