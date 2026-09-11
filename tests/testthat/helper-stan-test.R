# Helper functions for Stan unit test harnesses

find_stan_include_dir <- function() {
  # 1. Check installed package library (standard during R CMD check)
  inst_dir <- system.file("stan", package = "imuGAP")
  if (nzchar(inst_dir) && dir.exists(inst_dir)) {
    return(normalizePath(inst_dir))
  }

  # 2. Check source directory via testthat::test_path
  dev_dir <- tryCatch(
    normalizePath(testthat::test_path("../../inst/stan"), mustWork = TRUE),
    error = function(e) ""
  )
  if (nzchar(dev_dir) && dir.exists(dev_dir)) {
    return(dev_dir)
  }

  # 3. Fallback: check getwd()/inst/stan
  src_dir <- file.path(getwd(), "inst", "stan")
  if (dir.exists(src_dir)) {
    return(normalizePath(src_dir))
  }

  stop("Could not locate inst/stan include directory")
}

is_full_check <- function() {
  nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")) ||
    nzchar(Sys.getenv("R_CMD_CHECK")) ||
    identical(Sys.getenv("CI"), "true") ||
    identical(Sys.getenv("IMUGAP_TEST_STAN_FORCE"), "true")
}

get_stan_test_cache <- function() {
  cache_file <- file.path(tempdir(), ".imugap_stan_test_cache.rds")
  if (file.exists(cache_file)) {
    tryCatch(readRDS(cache_file), error = function(e) list())
  } else {
    list()
  }
}

update_stan_test_cache <- function(key, hashes) {
  cache_file <- file.path(tempdir(), ".imugap_stan_test_cache.rds")
  current <- get_stan_test_cache()
  current[[key]] <- hashes
  tryCatch(saveRDS(current, cache_file), error = function(e) NULL)
}

.initial_stan_test_cache <- if (!is_full_check()) {
  get_stan_test_cache()
} else {
  list()
}

skip_if_stan_unchanged <- function(include_relpaths) {
  # Never skip and never touch cache in full check / CI mode
  if (is_full_check()) {
    return(invisible(TRUE))
  }

  stan_dir <- find_stan_include_dir()
  abs_paths <- file.path(stan_dir, include_relpaths)
  missing <- abs_paths[!file.exists(abs_paths)]
  if (length(missing) > 0) {
    testthat::skip(paste(
      "Stan include file not found:",
      paste(missing, collapse = ", ")
    ))
  }

  file_hashes <- vapply(
    abs_paths,
    function(p) {
      tools::md5sum(p)[[1]]
    },
    FUN.VALUE = character(1)
  )

  key <- paste(sort(include_relpaths), collapse = ";")
  cached_hashes <- .initial_stan_test_cache[[key]]

  if (!is.null(cached_hashes) && identical(cached_hashes, file_hashes)) {
    testthat::skip(paste(
      "Stan include(s) unchanged:",
      paste(include_relpaths, collapse = ", ")
    ))
  }

  # Update cache for successful future runs in local dev
  update_stan_test_cache(key, file_hashes)
  invisible(TRUE)
}

compile_stan_harness <- function(stan_code) {
  stan_dir <- find_stan_include_dir()
  tmp_stan <- tempfile(fileext = ".stan")
  on.exit(unlink(tmp_stan), add = TRUE)
  writeLines(stan_code, tmp_stan)

  suppressWarnings(suppressMessages(rstan::stan_model(
    file = tmp_stan,
    isystem = stan_dir,
    auto_write = FALSE,
    save_dso = FALSE
  )))
}

run_stan_harness <- function(
  model_or_code,
  data = list(),
  pars = NULL,
  seed = 42L,
  return_fit = FALSE
) {
  sm <- if (inherits(model_or_code, "stanmodel")) {
    model_or_code
  } else {
    compile_stan_harness(model_or_code)
  }

  out_lines <- character(0)
  msg_lines <- character(0)
  out_lines <- utils::capture.output(
    msg_lines <- utils::capture.output(
      fit <- suppressWarnings(rstan::sampling(
        sm,
        data = data,
        algorithm = "Fixed_param",
        iter = 1,
        warmup = 0,
        chains = 1,
        refresh = 0,
        seed = seed,
        show_messages = TRUE
      )),
      type = "message"
    ),
    type = "output"
  )

  if (return_fit) {
    return(fit)
  }

  if (fit@mode != 0L) {
    all_output <- paste(c(out_lines, msg_lines), collapse = "\n")
    stop(
      "Stan execution failed (fit@mode == ",
      fit@mode,
      "):\n",
      all_output
    )
  }

  pars_expr <- substitute(pars)
  pars_val <- tryCatch(
    if (is.null(pars)) NULL else as.character(pars),
    error = function(e) as.character(pars_expr)
  )

  reshape_single_iter <- function(x) {
    d <- dim(x)
    if (is.null(d)) {
      return(x)
    }
    if (length(d) == 1L) {
      return(x[[1]])
    }
    if (length(d) == 2L) {
      return(as.vector(x))
    }
    array(x, dim = d[-1])
  }

  if (is.null(pars_val)) {
    extracted <- rstan::extract(fit)
    return(lapply(extracted, reshape_single_iter))
  }

  extracted <- rstan::extract(fit, pars = pars_val)
  if (length(pars_val) == 1L) {
    return(reshape_single_iter(extracted[[pars_val]]))
  }
  lapply(extracted[pars_val], reshape_single_iter)
}

empty_obs_stream <- function(tag = c("uncensored", "right", "left")) {
  tag <- match.arg(tag)
  setNames(
    list(
      0L,
      integer(0),
      integer(0),
      0L,
      integer(0),
      integer(0),
      integer(0),
      integer(0),
      integer(0),
      numeric(0)
    ),
    paste0(
      c(
        "n_obs_",
        "y_obs_",
        "y_smp_",
        "n_weights_",
        "obs_to_weights_bounds_",
        "weights_cohort_",
        "weights_location_",
        "weights_dose_",
        "weights_life_year_",
        "weights_"
      ),
      tag
    )
  )
}
