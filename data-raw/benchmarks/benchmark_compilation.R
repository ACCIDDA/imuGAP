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

pkgload::load_all(
  path = pkgload::pkg_path(),
  compile = FALSE,
  export_all = TRUE,
  quiet = TRUE
)
SOLVER_TYPE_MAP <- solver_type_map
GUESS_TYPE_MAP <- guess_type_map

# --- Configuration Reading & Normalization -----------------------------------

normalize_benchmark_config <- function(cfg) {
  if (is.null(cfg$links)) {
    cfg$links <- c("logit", "probit")
  } else {
    cfg$links <- as.character(cfg$links)
  }

  if (is.null(cfg$guessers)) {
    cfg$guessers <- names(GUESS_TYPE_MAP)
  } else {
    cfg$guessers <- as.character(cfg$guessers)
  }

  if (is.null(cfg$solvers)) {
    cfg$solvers <- names(SOLVER_TYPE_MAP)
  } else {
    cfg$solvers <- as.character(cfg$solvers)
  }

  cfg
}

read_benchmark_config <- function(config_path) {
  stopifnot(file.exists(config_path))
  normalize_benchmark_config(yaml::read_yaml(config_path))
}

# --- Combination Resolver -----------------------------------------------------

get_valid_model_combinations <- function(cfg) {
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
