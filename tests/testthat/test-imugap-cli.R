# Tests for inst/scripts/imugap.R

script_path <- system.file("scripts", "imugap.R", package = "imuGAP")
if (script_path == "") {
  script_path <- file.path("../../inst/scripts/imugap.R")
}

script_lines <- readLines(script_path)
fn_start <- grep("^find_input_file <- function", script_lines)[1]
main_end <- grep("^if \\(!interactive\\(\\)\\)", script_lines)[1] - 1
eval(parse(text = script_lines[fn_start:main_end]))

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