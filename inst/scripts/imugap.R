#!/usr/bin/env Rscript

# --- Usage -------------------------------------------------------------------

USAGE <- "imugap.R — Minimal CLI for imuGAP model fitting

Usage: imugap <input_dir> [output_dir]
       imugap -h <input_dir>          (validate only, no model fitting)
       imugap -h | --help             (show this message)

input_dir must contain:
  observations.csv (or .rds)      — columns: obs_id, positive, sample_n
  populations.csv (or .rds)       — columns: obs_id, loc_id, cohort, age, dose, weight
  locations.csv (or .rds)         — columns: loc_id, parent_id (hierarchical; see package docs)

Output: fit.rds (raw stanfit object for post-processing).
output_dir defaults to input_dir. Exit codes: 0=success, 1=validation, 2=model, 3=I/O.
"

# --- Package guard -----------------------------------------------------------

if (!requireNamespace("imuGAP", quietly = TRUE)) {
  stop("Package 'imuGAP' required. Install with: remotes::install_github(\"ACCIDDA/imuGAP\")")
}

# --- Helpers -----------------------------------------------------------------

SUPPORTED_EXT <- c("csv", "rds")

file_exists_any_ext <- function(dir, name) {
  any(file.exists(file.path(dir, paste0(name, ".", SUPPORTED_EXT))))
}

load_by_ext <- function(path) {
  ext <- tolower(tools::file_ext(path))
  tryCatch(
    switch(ext,
      csv = read.csv(path, stringsAsFactors = FALSE),
      rds = readRDS(path),
      stop("Unsupported extension '.", ext, "'", call. = FALSE)
    ),
    error = function(e) {
      stop("Failed to read '", basename(path), "': ", e$message, call. = FALSE)
    }
  )
}

# CSV takes precedence over RDS when both exist
find_input_file <- function(dir, name) {
  for (ext in SUPPORTED_EXT) {
    path <- file.path(dir, paste0(name, ".", ext))
    if (file.exists(path)) return(load_by_ext(path))
  }
  stop("Expected '", name, ".csv' or '", name, ".rds' in ", dir, "/",
       call. = FALSE)
}

# Reports all missing files at once rather than failing on the first
check_all_inputs <- function(dir) {
  required <- c("observations", "populations", "locations")
  missing <- required[!vapply(required, function(n) file_exists_any_ext(dir, n), logical(1))]
  if (length(missing) > 0) {
    stop("Missing input files in ", dir, "/: ",
         paste(missing, collapse = ", "),
         " (expected .csv or .rds)", call. = FALSE)
  }
}

# --- Main --------------------------------------------------------------------

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  help_flag <- length(args) > 0 && args[1] %in% c("-h", "--help")

  if (length(args) == 0 || (help_flag && length(args) == 1)) {
    cat(USAGE)
    return(0)
  }

  if (help_flag) {
    input_dir <- args[2]
    output_dir <- input_dir
  } else {
    input_dir <- args[1]
    output_dir <- if (length(args) >= 2) args[2] else input_dir
  }

  if (!dir.exists(input_dir)) {
    message("ERROR: Input directory not found: ", input_dir)
    return(3)
  }

  err <- tryCatch({ check_all_inputs(input_dir); NULL }, error = identity)
  if (!is.null(err)) {
    message("ERROR: ", err$message)
    return(3)
  }

  message("[\u2192] Loading inputs...")
  inputs <- tryCatch(
    list(
      obs     = find_input_file(input_dir, "observations"),
      pops    = find_input_file(input_dir, "populations"),
      locs    = find_input_file(input_dir, "locations")
    ),
    error = identity
  )
  if (inherits(inputs, "error")) {
    message("ERROR: ", inputs$message)
    return(3)
  }
  message("[\u2713] Inputs loaded.")

  message("[\u2192] Validating schema...")
  canonical <- tryCatch({
    locs <- imuGAP::canonicalize_locations(inputs$locs)
    obs  <- imuGAP::canonicalize_observations(inputs$obs)
    pops <- imuGAP::canonicalize_populations(inputs$pops, obs, locs)
    list(locs = locs, obs = obs, pops = pops)
  }, error = identity)
  if (inherits(canonical, "error")) {
    message("ERROR: ", canonical$message)
    return(1)
  }
  message("[\u2713] Schema validated.")

  if (help_flag) {
    message("[\u2713] Validation passed.")
    return(0)
  }

  stan_opts <- imuGAP::stan_options(iter = 2000L, chains = 4L)

  if (!dir.exists(output_dir)) {
    ok <- dir.create(output_dir, recursive = TRUE)
    if (!ok) {
      message("ERROR: Could not create output directory: ", output_dir)
      return(3)
    }
  }

  message("[\u2192] Launching imuGAP...")
  fit <- tryCatch(
    imuGAP::imuGAP(
      observations = canonical$obs, populations = canonical$pops,
      locations = canonical$locs,
      imugap_opts = imuGAP::imugap_options(df = 5L, dose_schedule = c(1L, 4L)),
      stan_opts = stan_opts
    ),
    error = identity
  )
  if (inherits(fit, "error")) {
    message("ERROR: ", fit$message)
    return(2)
  }
  message("[\u2713] Model complete.")

  fit_path <- file.path(output_dir, "fit.rds")
  save_err <- tryCatch({ saveRDS(fit, fit_path); NULL }, error = identity)
  if (!is.null(save_err)) {
    message("ERROR: Failed to save output to ", fit_path, ": ", save_err$message)
    return(3)
  }
  message("[\u2713] Wrote ", fit_path)

  return(0)
}

# --- Entry guard -------------------------------------------------------------

if (!interactive()) {
  status <- main()
  quit(status = status, save = "no")
}
