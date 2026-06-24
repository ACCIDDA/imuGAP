#' @title Stan Sampler Options
#'
#' @description
#' Collects and validates sampler arguments for the chosen `backend`, forwarding
#' them **verbatim** so calls feel native to that backend. Use the backend's own
#' argument names; mixing one backend's vocabulary into the other errors with a
#' hint. The model object is supplied separately via `imugap_options()`, and
#' `data` is constructed internally, so neither may be set here.
#'
#' @param ... sampler arguments forwarded verbatim to the chosen backend's
#'   sampler. Use the backend's own names: for `"rstan"`, the
#'   [rstan::sampling()] arguments (`iter`, `chains`, `cores`, `seed`); for
#'   `"cmdstanr"`, the `$sample()` arguments (`iter_warmup`, `iter_sampling`,
#'   `parallel_chains`, ...).
#' @param backend which Stan interface to target, one of `"rstan"` (default) or
#'   `"cmdstanr"`. Determines which argument vocabulary is accepted and which
#'   sampler [sampling()] calls. Selecting `"cmdstanr"` errors if the cmdstanr
#'   package is not installed.
#'
#' @examples
#' stan_options()
#' stan_options(chains = 2, iter = 500)
#' if (requireNamespace("cmdstanr", quietly = TRUE)) {
#'   stan_options(backend = "cmdstanr", parallel_chains = 4, iter_warmup = 500)
#' }
#'
#' @return a named list of validated sampler arguments, tagged with the backend
#'   it was built for
#' @export
stan_options <- function(..., backend = "rstan") {
  backend <- match.arg(backend, c("rstan", "cmdstanr"))
  check_backend_available(backend)
  res <- list(...)
  if ("object" %in% names(res)) {
    stop(
      "Passing 'object' in stan_options is not allowed; ",
      "The model object should be passed in `imugap_options` instead."
    )
  }
  if ("data" %in% names(res)) {
    stop(
      "Passing 'data' in stan_options is not allowed; ",
      "The `sampling` or `predict` functions construct the 'data' argument internally."
    )
  }

  # Reject the other backend's vocabulary with a "did you mean" hint.
  check_backend_vocab(names(res), backend)

  # Validate the positive-integer count arguments native to this backend.
  int_args <- if (backend == "rstan") {
    c("iter", "chains", "warmup", "cores")
  } else {
    c("iter_warmup", "iter_sampling", "thin", "parallel_chains", "chains")
  }
  for (arg in intersect(names(res), int_args)) {
    if (length(res[[arg]]) != 1L) {
      stop(
        sprintf("'%s' must be a single positive integer", arg),
        call. = FALSE
      )
    }
    res[[arg]] <- check_positive_int(res[[arg]], arg)
  }
  if ("seed" %in% names(res)) {
    val <- res[["seed"]]
    if (length(val) != 1L) {
      stop("'seed' must be a single value", call. = FALSE)
    }
    val <- suppressWarnings(as.integer(val))
    if (is.na(val)) {
      stop("'seed' must be coercible to an integer", call. = FALSE)
    }
    res[["seed"]] <- val
  }
  attr(res, "stan_backend") <- backend
  res
}

#' @title imuGAP Model Options
#'
#' @description
#' This function encapsulates option passing for imuGAP settings.
#'
#' @param df degrees of freedom to use in bspline
#' @param dose_schedule an integer vector, the ages at which dose(s) `n` are
#'   scheduled, with vector indices and doses matching
#' @param object which stan model object to use; currently only "default" is
#'   supported
#'
#' @examples
#' imugap_options()
#' imugap_options(dose_schedule = c(1, 3))
#'
#' @return a list of imuGAP model options
#' @export
imugap_options <- function(
  df = 5L,
  dose_schedule = c(1, 4),
  object = c("default")
) {
  if (length(df) != 1L) {
    stop("'df' must be a single positive integer", call. = FALSE)
  }
  df <- check_positive_int(df, "df")

  dose_schedule <- check_positive_int(dose_schedule, "dose_schedule")
  if (is.unsorted(dose_schedule, strictly = TRUE)) {
    stop(
      "'dose_schedule' must be an ascending vector of positive integers",
      call. = FALSE
    )
  }

  object <- switch(
    object,
    "default" = stanmodels$impute_school_coverage_process_v6,
    stop("Unknown model object: ", object)
  )
  as.list(environment())
}
