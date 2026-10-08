#' Model Assembly and Compilation Manager for imuGAP Benchmarking
#'
#' Usage:
#'   Rscript benchmark_compilation.R <config.yml> [--generate-only | --compile | --unified | --parallel]
#'
#' 1. Reads a benchmark configuration file (specifying links, guessers, and solvers).
#' 2. Assembles intermediate Stan models into `data-raw/benchmodels/`.
#' 3. Compiles and caches Stan DSO objects into `data-raw/benchobjs/` (with parallel execution).

suppressPackageStartupMessages({
  library(yaml)
  library(rstan)
  library(imuGAP)
  library(parallel)
})

# --- Stan Directory Resolution ------------------------------------------------

find_stan_include_dir <- function() {
  src_dir <- file.path(getwd(), "inst", "stan")
  if (dir.exists(src_dir)) {
    return(normalizePath(src_dir))
  }
  parent_src <- file.path(dirname(getwd()), "inst", "stan")
  if (dir.exists(parent_src)) {
    return(normalizePath(parent_src))
  }
  grandparent_src <- file.path(dirname(dirname(getwd())), "inst", "stan")
  if (dir.exists(grandparent_src)) {
    return(normalizePath(grandparent_src))
  }
  inst_dir <- system.file("stan", package = "imuGAP")
  if (nzchar(inst_dir) && dir.exists(inst_dir)) {
    return(normalizePath(inst_dir))
  }
  stop("Could not locate inst/stan include directory")
}

# --- Type Mappings ------------------------------------------------------------

GUESS_TYPE_MAP <- c(
  zero = 1L,
  taylor2 = 2L,
  taylor4 = 3L,
  pade = 4L,
  asymptotic = 5L,
  conditioned = 6L
)

SOLVER_TYPE_MAP <- c(
  direct = 1L,
  halley2 = 2L,
  newton2 = 3L,
  builtin = 4L,
  halley10 = 5L
)

# --- Combination Resolver -----------------------------------------------------

get_valid_model_combinations <- function(cfg) {
  links <- if (!is.null(cfg$links)) {
    as.character(cfg$links)
  } else {
    c("logit", "probit")
  }
  guessers <- if (!is.null(cfg$guessers)) {
    as.character(cfg$guessers)
  } else {
    c("zero", "taylor2", "taylor4", "pade", "asymptotic", "conditioned")
  }
  solvers <- if (!is.null(cfg$solvers)) {
    as.character(cfg$solvers)
  } else {
    c("direct", "halley2", "newton2", "builtin", "halley10")
  }

  models <- list()
  for (lnk in links) {
    for (gss in guessers) {
      if (lnk == "logit" && gss == "pade") {
        next
      }

      for (slv in solvers) {
        model_name <- sprintf("model_%s_%s_%s", lnk, gss, slv)
        label <- sprintf(
          "%s %s (%s)",
          tools::toTitleCase(lnk),
          tools::toTitleCase(gss),
          tools::toTitleCase(slv)
        )
        models[[length(models) + 1L]] <- list(
          link = lnk,
          guess = gss,
          guess_type = unname(GUESS_TYPE_MAP[gss]),
          solver = slv,
          solver_type = unname(SOLVER_TYPE_MAP[slv]),
          model_name = model_name,
          label = label
        )
      }
    }
  }
  models
}

parse_model_tag <- function(tag) {
  clean_tag <- sub("^model_", "", tag)
  parts <- strsplit(clean_tag, "_")[[1L]]
  if (length(parts) < 3L) {
    stop(sprintf(
      "Invalid model tag format: '%s' (expected <link>_<guess>_<solver>)",
      tag
    ))
  }
  list(
    link = parts[1L],
    guess = parts[2L],
    solver = paste(parts[3:length(parts)], collapse = "_"),
    model_name = sprintf("model_%s", clean_tag)
  )
}

# --- Path Query Helpers for Makefile ------------------------------------------

get_model_stan_paths <- function(config_path, benchmodels_dir = "benchmodels") {
  stopifnot(file.exists(config_path))
  cfg <- yaml::read_yaml(config_path)
  models <- get_valid_model_combinations(cfg)
  paths <- vapply(
    models,
    function(m) file.path(benchmodels_dir, sprintf("%s.stan", m$model_name)),
    character(1)
  )
  paste(paths, collapse = " ")
}

get_model_obj_paths <- function(config_path, benchobjs_dir = "benchobjs") {
  stopifnot(file.exists(config_path))
  cfg <- yaml::read_yaml(config_path)
  models <- get_valid_model_combinations(cfg)
  paths <- vapply(
    models,
    function(m) file.path(benchobjs_dir, sprintf("%s.rds", m$model_name)),
    character(1)
  )
  paste(paths, collapse = " ")
}

get_unified_model_stan_paths <- function(
  config_path = NULL,
  benchmodels_dir = "benchmodels"
) {
  links <- if (!is.null(config_path) && file.exists(config_path)) {
    cfg <- yaml::read_yaml(config_path)
    if (!is.null(cfg$links)) as.character(cfg$links) else c("logit", "probit")
  } else {
    c("logit", "probit")
  }
  paths <- vapply(
    links,
    function(lnk) {
      file.path(benchmodels_dir, sprintf("model_unified_%s.stan", lnk))
    },
    character(1)
  )
  paste(paths, collapse = " ")
}

get_unified_model_obj_paths <- function(
  config_path = NULL,
  benchobjs_dir = "benchobjs"
) {
  links <- if (!is.null(config_path) && file.exists(config_path)) {
    cfg <- yaml::read_yaml(config_path)
    if (!is.null(cfg$links)) as.character(cfg$links) else c("logit", "probit")
  } else {
    c("logit", "probit")
  }
  paths <- vapply(
    links,
    function(lnk) {
      file.path(benchobjs_dir, sprintf("model_unified_%s.rds", lnk))
    },
    character(1)
  )
  paste(paths, collapse = " ")
}

# --- Stan Template Assembly ---------------------------------------------------

generate_model_stan <- function(
  link,
  guess,
  solver,
  benchmodels_dir = "benchmodels"
) {
  if (!dir.exists(benchmodels_dir)) {
    dir.create(benchmodels_dir, recursive = TRUE, showWarnings = FALSE)
  }

  stan_dir <- find_stan_include_dir()
  tmpl_path <- file.path(
    stan_dir,
    "templates",
    "bspline_static_offsets.stan.template"
  )
  stopifnot(file.exists(tmpl_path))

  tmpl_lines <- readLines(tmpl_path, warn = FALSE)
  stan_code <- paste(tmpl_lines, collapse = "\n") |>
    gsub(pattern = "@LINK@", replacement = link, fixed = TRUE) |>
    gsub(pattern = "@GUESS@", replacement = guess, fixed = TRUE) |>
    gsub(pattern = "@SOLVE@", replacement = solver, fixed = TRUE)

  model_file <- file.path(
    benchmodels_dir,
    sprintf("model_%s_%s_%s.stan", link, guess, solver)
  )
  writeLines(stan_code, model_file)
  model_file
}

generate_unified_model_stan <- function(link, benchmodels_dir = "benchmodels") {
  if (!dir.exists(benchmodels_dir)) {
    dir.create(benchmodels_dir, recursive = TRUE, showWarnings = FALSE)
  }

  stan_code <- sprintf(
    paste(
      "functions {",
      "  #include functions/convenience.stan",
      "  #include functions/link/%s.stan",
      "  #include functions/guess/dispatch_%s.stan",
      "  #include functions/solvers/dispatch.stan",
      "  #include functions/layer_offsets.stan",
      "  #include functions/unrolled_dose_static_lambda.stan",
      "  #include functions/observation_likelihood_reduce.stan",
      "}",
      "data {",
      "  #include data/shared.stan",
      "  #include data/locations.stan",
      "  #include data/uncensored/weights_location.stan",
      "  #include data/right/weights_location.stan",
      "  #include data/left/weights_location.stan",
      "  #include data/bspline.stan",
      "}",
      "transformed data {",
      "  #include transformed_data/common_indices.stan",
      "  #include transformed_data/right/observations.stan",
      "  #include transformed_data/layer_indices.stan",
      "  #include transformed_data/layer_phi_lookup.stan",
      "}",
      "parameters {",
      "  #include parameters/bspline.stan",
      "  #include parameters/layer_offsets.stan",
      "  #include parameters/static_lambda.stan",
      "}",
      "transformed parameters {",
      "  #include transformed_parameters/bspline.stan",
      "  #include transformed_parameters/layer_offsets.stan",
      "}",
      "model {",
      "  if (!predict_mode) {",
      "    #include model/bspline.stan",
      "    #include model/static_lambda.stan",
      "    #include model/layer_offsets.stan",
      "    #include model/hierarchical_phi.stan",
      "    #include model/observation_likelihood.stan",
      "  }",
      "}",
      "generated quantities {",
      "  vector[predict_mode ? n_obs : 0] p_obs;",
      "  vector[compute_log_lik ? n_obs : 0] log_lik;",
      "  if (predict_mode || compute_log_lik) {",
      "    #include model/hierarchical_phi.stan",
      "    if (predict_mode) {",
      "      #include generated_quantities/pointwise_p_obs.stan",
      "    }",
      "    if (compute_log_lik) {",
      "      #include generated_quantities/pointwise_log_lik.stan",
      "    }",
      "  }",
      "}",
      "",
      sep = "\n"
    ),
    link,
    link
  )

  model_file <- file.path(
    benchmodels_dir,
    sprintf("model_unified_%s.stan", link)
  )
  writeLines(stan_code, model_file)
  model_file
}

generate_single_model_stan <- function(tag, benchmodels_dir = "benchmodels") {
  if (grepl("^unified_", tag) || grepl("^model_unified_", tag)) {
    link <- sub("^model_", "", sub("^unified_", "", tag))
    link <- sub("^unified_", "", link)
    generate_unified_model_stan(link, benchmodels_dir = benchmodels_dir)
  } else {
    parsed <- parse_model_tag(tag)
    generate_model_stan(
      link = parsed$link,
      guess = parsed$guess,
      solver = parsed$solver,
      benchmodels_dir = benchmodels_dir
    )
  }
}

# --- Model Compilation & Caching ----------------------------------------------

compile_or_load_unified_model <- function(
  link,
  benchmodels_dir = "benchmodels",
  benchobjs_dir = "benchobjs",
  force = FALSE
) {
  if (!dir.exists(benchobjs_dir)) {
    dir.create(benchobjs_dir, recursive = TRUE, showWarnings = FALSE)
  }

  model_name <- sprintf("model_unified_%s", link)
  obj_path <- file.path(benchobjs_dir, sprintf("%s.rds", model_name))
  stan_path <- file.path(benchmodels_dir, sprintf("%s.stan", model_name))

  if (!file.exists(stan_path)) {
    generate_unified_model_stan(link, benchmodels_dir = benchmodels_dir)
  }

  if (!force && file.exists(obj_path)) {
    model <- tryCatch(readRDS(obj_path), error = function(e) NULL)
    if (inherits(model, "stanmodel")) {
      return(model)
    }
  }

  cat(sprintf("Compiling unified model [%s] ... ", model_name))
  flush(stdout())
  stan_dir <- find_stan_include_dir()

  t_start <- proc.time()
  model <- rstan::stan_model(
    file = stan_path,
    isystem = stan_dir,
    auto_write = FALSE,
    save_dso = TRUE,
    verbose = FALSE
  )
  t_elapsed <- (proc.time() - t_start)[["elapsed"]]

  saveRDS(model, obj_path)
  cat(sprintf("done (%.1fs) -> '%s'\n", t_elapsed, obj_path))
  flush(stdout())

  invisible(model)
}

compile_or_load_model <- function(
  link,
  guess,
  solver,
  benchmodels_dir = "benchmodels",
  benchobjs_dir = "benchobjs",
  force = FALSE
) {
  if (!dir.exists(benchobjs_dir)) {
    dir.create(benchobjs_dir, recursive = TRUE, showWarnings = FALSE)
  }

  model_name <- sprintf("model_%s_%s_%s", link, guess, solver)
  obj_path <- file.path(benchobjs_dir, sprintf("%s.rds", model_name))
  stan_path <- file.path(benchmodels_dir, sprintf("%s.stan", model_name))

  if (!file.exists(stan_path)) {
    generate_model_stan(link, guess, solver, benchmodels_dir = benchmodels_dir)
  }

  if (!force && file.exists(obj_path)) {
    model <- tryCatch(readRDS(obj_path), error = function(e) NULL)
    if (inherits(model, "stanmodel")) {
      return(model)
    }
  }

  cat(sprintf("Compiling model [%s] ... ", model_name))
  flush(stdout())
  stan_dir <- find_stan_include_dir()

  t_start <- proc.time()
  model <- rstan::stan_model(
    file = stan_path,
    isystem = stan_dir,
    auto_write = FALSE,
    save_dso = TRUE,
    verbose = FALSE
  )
  t_elapsed <- (proc.time() - t_start)[["elapsed"]]

  saveRDS(model, obj_path)
  cat(sprintf("done (%.1fs) -> '%s'\n", t_elapsed, obj_path))
  flush(stdout())

  invisible(model)
}

get_or_compile_cached_model <- compile_or_load_model
get_or_compile_unified_model <- compile_or_load_unified_model

compile_single_model <- function(
  tag,
  benchmodels_dir = "benchmodels",
  benchobjs_dir = "benchobjs",
  force = FALSE
) {
  if (grepl("unified", tag)) {
    link <- sub("^model_", "", sub("^unified_", "", tag))
    link <- sub("^unified_", "", link)
    compile_or_load_unified_model(
      link = link,
      benchmodels_dir = benchmodels_dir,
      benchobjs_dir = benchobjs_dir,
      force = force
    )
  } else {
    parsed <- parse_model_tag(tag)
    compile_or_load_model(
      link = parsed$link,
      guess = parsed$guess,
      solver = parsed$solver,
      benchmodels_dir = benchmodels_dir,
      benchobjs_dir = benchobjs_dir,
      force = force
    )
  }
}

# --- Batch Manager ------------------------------------------------------------

process_benchmark_models <- function(
  config_path,
  compile_now = FALSE,
  unified_only = TRUE,
  parallel_compile = TRUE,
  n_cores = parallel::detectCores(),
  benchmodels_dir = "benchmodels",
  benchobjs_dir = "benchobjs"
) {
  stopifnot(file.exists(config_path))
  cfg <- yaml::read_yaml(config_path)
  links <- if (!is.null(cfg$links)) {
    as.character(cfg$links)
  } else {
    c("logit", "probit")
  }

  if (isTRUE(unified_only)) {
    cat(sprintf(
      "Generating %d unified models in '%s/'...\n",
      length(links),
      benchmodels_dir
    ))
    flush(stdout())
    stan_files <- vapply(
      links,
      function(lnk) {
        generate_unified_model_stan(lnk, benchmodels_dir = benchmodels_dir)
      },
      character(1)
    )

    if (isTRUE(compile_now)) {
      cat(sprintf(
        "Compiling unified models into '%s/' (parallel=%s)...\n",
        benchobjs_dir,
        parallel_compile
      ))
      flush(stdout())
      if (isTRUE(parallel_compile) && length(links) > 1L) {
        parallel::mclapply(
          links,
          function(lnk) {
            compile_or_load_unified_model(
              link = lnk,
              benchmodels_dir = benchmodels_dir,
              benchobjs_dir = benchobjs_dir
            )
          },
          mc.cores = min(length(links), n_cores)
        )
      } else {
        for (lnk in links) {
          compile_or_load_unified_model(
            link = lnk,
            benchmodels_dir = benchmodels_dir,
            benchobjs_dir = benchobjs_dir
          )
        }
      }
    }
    return(invisible(list(links = links, stan_files = stan_files)))
  }

  models <- get_valid_model_combinations(cfg)
  cat(sprintf(
    "Configuration '%s': %d valid model combinations.\n",
    config_path,
    length(models)
  ))
  flush(stdout())

  stan_files <- character(length(models))
  for (i in seq_along(models)) {
    m <- models[[i]]
    stan_files[i] <- generate_model_stan(
      link = m$link,
      guess = m$guess,
      solver = m$solver,
      benchmodels_dir = benchmodels_dir
    )
  }
  cat(sprintf(
    "Generated %d template files in '%s/'.\n",
    length(stan_files),
    benchmodels_dir
  ))

  if (isTRUE(compile_now)) {
    cat(sprintf(
      "Compiling %d models into '%s/' (parallel=%s)...\n",
      length(models),
      benchobjs_dir,
      parallel_compile
    ))
    flush(stdout())
    if (isTRUE(parallel_compile)) {
      parallel::mclapply(
        models,
        function(m) {
          compile_or_load_model(
            link = m$link,
            guess = m$guess,
            solver = m$solver,
            benchmodels_dir = benchmodels_dir,
            benchobjs_dir = benchobjs_dir
          )
        },
        mc.cores = min(length(models), n_cores)
      )
    } else {
      for (i in seq_along(models)) {
        m <- models[[i]]
        compile_or_load_model(
          link = m$link,
          guess = m$guess,
          solver = m$solver,
          benchmodels_dir = benchmodels_dir,
          benchobjs_dir = benchobjs_dir
        )
      }
    }
  }

  invisible(list(models = models, stan_files = stan_files))
}

# --- CLI Entrypoint -----------------------------------------------------------

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0L) {
    cat(
      "Usage: Rscript benchmark_compilation.R <config.yml> [--compile | --generate-only | --unified]\n"
    )
    cat("Defaulting to config_stress_test.yml (unified, compile)...\n")
    config_file <- if (file.exists("config_stress_test.yml")) {
      "config_stress_test.yml"
    } else {
      "data-raw/benchmarks/config_stress_test.yml"
    }
    do_compile <- TRUE
    unified_mode <- TRUE
  } else {
    config_file <- args[1L]
    do_compile <- ("--compile" %in% args) || (!("--generate-only" %in% args))
    unified_mode <- !("--standalone" %in% args)
  }

  in_benchmarks <- dir.exists("benchmodels") || grepl("benchmarks/?$", getwd())
  models_dir <- if (in_benchmarks) {
    "benchmodels"
  } else {
    "data-raw/benchmarks/benchmodels"
  }
  objs_dir <- if (in_benchmarks) {
    "benchobjs"
  } else {
    "data-raw/benchmarks/benchobjs"
  }

  process_benchmark_models(
    config_path = config_file,
    compile_now = do_compile,
    unified_only = unified_mode,
    parallel_compile = TRUE,
    benchmodels_dir = models_dir,
    benchobjs_dir = objs_dir
  )
}
