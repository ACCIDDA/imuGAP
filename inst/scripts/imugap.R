#!/usr/bin/env Rscript

# --- Usage -------------------------------------------------------------------

USAGE <- "imugap.R — Minimal CLI for imuGAP model fitting

Usage: imugap <input_dir> [output_dir]
       imugap -h <input_dir>          (validate only)

input_dir must contain:
  observations.csv (or .rds)      — columns: positive, sample_n
  obs_populations.csv (or .rds)   — columns: obs_id, location, cohort, age, dose, weight
  locations.csv (or .rds)         — columns: id, parent_id

Output: fit.rds (raw stanfit object for post-processing).
output_dir defaults to input_dir. Exit codes: 0=success, 1=validation, 2=model, 3=I/O.
"

# --- Package guard -----------------------------------------------------------

if (!requireNamespace("imuGAP", quietly = TRUE)) {
  stop("Package 'imuGAP' required. Install with: devtools::install()")
}

# --- Helpers -----------------------------------------------------------------

find_input_file <- function(dir, name) {
  csv_path <- file.path(dir, paste0(name, ".csv"))
  rds_path <- file.path(dir, paste0(name, ".rds"))

  if (file.exists(csv_path)) {
    return(read.csv(csv_path, stringsAsFactors = FALSE))
  }
  if (file.exists(rds_path)) {
    return(readRDS(rds_path))
  }

  stop("Expected '", name, ".csv' or '", name, ".rds' in ", dir, "/",
       call. = FALSE)
}

# --- Main --------------------------------------------------------------------

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) == 0 || (length(args) == 1 && args[1] == "-h")) {
    cat(USAGE)
    quit(save = "no")
  }

  dry_run <- args[1] == "-h"
  if (dry_run) {
    input_dir <- args[2]
  } else {
    input_dir <- args[1]
    output_dir <- if (length(args) >= 2) args[2] else input_dir
  }

  if (!dir.exists(input_dir)) {
    message("Error: Input directory not found: ", input_dir)
    quit(status = 3, save = "no")  # I/O error
  }

  # Load input files
  tryCatch({
    obs_raw     <- find_input_file(input_dir, "observations")
    obs_pop_raw <- find_input_file(input_dir, "obs_populations")
    locs_raw    <- find_input_file(input_dir, "locations")
  }, error = function(e) { message("ERROR: ", e$message); quit(status = 3, save = "no") })
  message("[\u2713] Loading inputs...")

  # Validate — assign to globalenv to work around eval(substitute()) NSE
  # in check_locations/checked_dt_able, which walks parent frames to resolve
  # symbols and can't find them across the imuGAP:: namespace boundary.
  .locs <- locs_raw; .obs <- obs_raw; .opop <- obs_pop_raw
  on.exit({ rm(.locs, .obs, .opop, envir = globalenv()) }, add = TRUE)
  assign(".locs", .locs, envir = globalenv())
  assign(".obs", .obs, envir = globalenv())
  assign(".opop", .opop, envir = globalenv())

  validated <- tryCatch({
    locs    <- imuGAP::check_locations(.locs)
    obs     <- imuGAP::check_observations(.obs)
    obs_pop <- imuGAP::check_obs_population(.opop, obs, locs)
    list(locs = locs, obs = obs, obs_pop = obs_pop)
  }, error = function(e) { message("ERROR: ", e$message); quit(status = 1, save = "no") })
  message("[\u2713] Validating schema...")

  if (dry_run) {
    message("Validation passed.")
    quit(save = "no")
  }

  # Fit model
  dose_schedule <- c(1L, 4L)
  stan_opts <- imuGAP::stan_options(iter = 2000L, chains = 4L)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  message("[\u2192] Launching imuGAP...")
  fit <- tryCatch(
    imuGAP::imuGAP(
      observations = validated$obs, obs_populations = validated$obs_pop,
      locations = validated$locs, dose_schedule = dose_schedule,
      imugap_opts = imuGAP::imugap_options(df = 5L),
      stan_opts = stan_opts
    ),
    error = function(e) { message("ERROR: ", e$message); quit(status = 2, save = "no") }
  )
  message("[\u2713] Model complete.")

  # Save raw model output for downstream post-processing
  fit_path <- file.path(output_dir, "fit.rds")
  saveRDS(fit, fit_path)
  message("[\u2713] Wrote ", fit_path)

  invisible(NULL)
}

# --- Entry guard -------------------------------------------------------------

if (!interactive()) main()
