#' Model Compilation Manager for imuGAP Benchmarking
#'
#' Usage:
#'   Rscript benchmark_compilation.R <stan_path> <output_path>
#'
#' 1. Reads a benchmark configuration file (specifying links, guessers, and solvers).
#' 2. Compiles and caches Stan DSO objects into `data-raw/benchobjs/` directly
#'    from shipped dispatch models (`inst/stan/bspline_static_zero_dispatch_<link>.stan`).

suppressPackageStartupMessages({
  library(yaml)
  library(rstan)
  library(parallel)
  library(pkgload)
})

# --- Package Environment Resolution -------------------------------------------

tryCatch(
  pkgload::load_all(
    path = pkgload::pkg_path(),
    compile = FALSE,
    export_all = TRUE,
    quiet = TRUE
  ),
  error = function(e) {
    r_dir <- pkgload::package_file("R")
    if (dir.exists(r_dir)) {
      r_files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
      r_files <- setdiff(
        r_files,
        file.path(r_dir, c("flexstanr.R", "stanmodels.R"))
      )
      for (f in r_files) {
        suppressWarnings(sys.source(f, envir = .GlobalEnv))
      }
    }
  }
)

SOLVER_TYPE_MAP <- if (exists("solver_type_map")) {
  solver_type_map
} else {
  c(direct = 1L, halley2 = 2L, newton2 = 3L, builtin = 4L, halley10 = 5L)
}

GUESS_TYPE_MAP <- if (exists("guess_type_map")) {
  guess_type_map
} else {
  c(
    zero = 1L,
    taylor2 = 2L,
    taylor4 = 3L,
    pade = 4L,
    asymptotic = 5L,
    conditioned = 6L
  )
}

# --- Configuration Reading & Normalization -----------------------------------

normalize_benchmark_config <- function(cfg) {
  if (is.null(cfg$links)) {
    cfg$links <- c("logit", "probit")
  } else {
    cfg$links <- as.character(cfg$links)
  }

  if (is.null(cfg$guessers)) {
    cfg$guessers <- "zero"
  } else {
    cfg$guessers <- as.character(cfg$guessers)
  }

  if (is.null(cfg$solvers)) {
    cfg$solvers <- names(SOLVER_TYPE_MAP)
  } else {
    cfg$solvers <- as.character(cfg$solvers)
  }

  if (is.null(cfg$seed)) {
    cfg$seed <- 42L
  } else {
    cfg$seed <- as.integer(cfg$seed)
  }

  if (is.null(cfg$n_sample_obs)) {
    cfg$n_sample_obs <- 1000L
  } else {
    cfg$n_sample_obs <- as.integer(cfg$n_sample_obs)
  }

  if (is.null(cfg$pop_per_sub)) {
    cfg$pop_per_sub <- 5000L
  } else {
    cfg$pop_per_sub <- as.integer(cfg$pop_per_sub)
  }

  raw_ages <- if (!is.null(cfg$obs_ages)) {
    cfg$obs_ages
  } else if (!is.null(cfg$obs_age)) {
    cfg$obs_age
  } else {
    c(2L, 4L)
  }
  cfg$obs_ages <- as.integer(raw_ages)
  cfg$obs_age <- NULL

  if (is.null(cfg$lambda_true)) {
    cfg$lambda_true <- 3.0
  } else {
    cfg$lambda_true <- as.numeric(cfg$lambda_true)
  }

  if (is.null(cfg$dose_schedule)) {
    cfg$dose_schedule <- 1L
  } else {
    cfg$dose_schedule <- as.integer(cfg$dose_schedule)
  }

  if (is.null(cfg$dose)) {
    cfg$dose <- 1L
  } else {
    cfg$dose <- as.integer(cfg$dose)
  }

  if (!is.null(cfg$k_grid)) {
    cfg$k_grid <- as.integer(cfg$k_grid)
  }

  if (!is.null(cfg$sigma_grid)) {
    cfg$sigma_grid <- as.numeric(cfg$sigma_grid)
  }

  if (!is.null(cfg$p0_trend)) {
    cfg$p0_trend <- as.numeric(cfg$p0_trend)
  }

  if (!is.null(cfg$n_samples)) {
    cfg$n_samples <- as.integer(cfg$n_samples)
  }

  cfg
}

read_benchmark_config <- function(config_path) {
  stopifnot(file.exists(config_path))
  normalize_benchmark_config(yaml::read_yaml(config_path))
}

# --- Combination Resolver -----------------------------------------------------

get_valid_model_combinations <- function(cfg) {
  unknown_guess <- setdiff(cfg$guessers, names(GUESS_TYPE_MAP))
  if (length(unknown_guess) > 0L) {
    stop(sprintf(
      "Guesser(s) not recognized: %s. Supported guessers are: %s.",
      paste(sprintf("'%s'", unknown_guess), collapse = ", "),
      paste(sprintf("'%s'", names(GUESS_TYPE_MAP)), collapse = ", ")
    ))
  }

  unknown_solver <- setdiff(cfg$solvers, names(SOLVER_TYPE_MAP))
  if (length(unknown_solver) > 0L) {
    stop(sprintf(
      "Solver(s) not recognized: %s. Supported solvers are: %s.",
      paste(sprintf("'%s'", unknown_solver), collapse = ", "),
      paste(sprintf("'%s'", names(SOLVER_TYPE_MAP)), collapse = ", ")
    ))
  }

  grid <- expand.grid(
    link = cfg$links,
    guess = cfg$guessers,
    solver = cfg$solvers,
    stringsAsFactors = FALSE
  )

  # Exclude combinations that are mathematically invalid (e.g., Pade is probit-only)
  grid <- grid[!(grid$link == "logit" & grid$guess == "pade"), , drop = FALSE]

  within(grid, {
    guess_type <- unname(GUESS_TYPE_MAP[guess])
    solver_type <- unname(SOLVER_TYPE_MAP[solver])
    model_name <- sprintf("model_%s_%s_%s", link, guess, solver)
    label <- sprintf(
      "%s %s (%s)",
      tools::toTitleCase(link),
      tools::toTitleCase(guess),
      tools::toTitleCase(solver)
    )
  })
}

# --- Stan Model Compilation ---------------------------------------------------

compile_unified_model <- function(stan_path, obj_path) {
  model_label <- tools::file_path_sans_ext(basename(obj_path))

  cat(sprintf("Compiling unified model [%s] ... ", model_label))
  flush(stdout())
  cxx_flags <- paste(Sys.getenv("CXXFLAGS"), "-DSTAN_THREADS -pthread")
  model <- withr::with_makevars(
    c(CPPFLAGS = "-DSTAN_THREADS", CXXFLAGS = cxx_flags),
    rstan::stan_model(
      file = stan_path,
      isystem = pkgload::package_file("inst", "stan"),
      auto_write = FALSE,
      save_dso = TRUE,
      verbose = FALSE
    )
  )
  saveRDS(model, obj_path)
  cat("done\n")
  invisible(model)
}

# --- CLI Entrypoint (Invoked by Make) -----------------------------------------

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  compile_unified_model(args[1L], args[2L])
}
