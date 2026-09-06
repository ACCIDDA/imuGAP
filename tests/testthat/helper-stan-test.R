# Helper functions for Stan unit test harnesses

find_stan_include_dir <- function() {
  # Check source directory (local dev / devtools::test())
  src_dir <- file.path(getwd(), "inst", "stan")
  if (dir.exists(src_dir)) {
    return(normalizePath(src_dir))
  }
  # Check installed package (R CMD check)
  inst_dir <- base::system.file("stan", package = "imuGAP")
  if (nzchar(inst_dir) && dir.exists(inst_dir)) {
    return(normalizePath(inst_dir))
  }
  # Fallback: search parent directories
  pkg_dir <- tryCatch(
    rprojroot::find_package_root_file("inst", "stan"),
    error = function(e) ""
  )
  if (nzchar(pkg_dir) && dir.exists(pkg_dir)) {
    return(normalizePath(pkg_dir))
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

run_stan_harness <- function(
  stan_code,
  data = list(),
  seed = 42L,
  return_fit = FALSE
) {
  stan_dir <- find_stan_include_dir()
  tmp_stan <- tempfile(fileext = ".stan")
  on.exit(unlink(tmp_stan), add = TRUE)
  writeLines(stan_code, tmp_stan)

  sm <- suppressWarnings(suppressMessages(rstan::stan_model(
    file = tmp_stan,
    isystem = stan_dir,
    auto_write = FALSE,
    save_dso = FALSE
  )))

  fit <- suppressWarnings(suppressMessages(rstan::sampling(
    sm,
    data = data,
    algorithm = "Fixed_param",
    iter = 1,
    warmup = 0,
    chains = 1,
    refresh = 0,
    seed = seed,
    show_messages = FALSE
  )))

  if (return_fit) {
    return(fit)
  }
  rstan::extract(fit)
}
