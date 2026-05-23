#' @title Stan Sampler Options
#'
#' @description
#' This function encapsulates option passing to the stan sampler, with the
#' exception of the model object, which is passed in `imugap_options`.
#'
#' @inheritDotParams rstan::sampling -object
#'
#' @examples
#' stan_options()
#' stan_options(chains = 2, iter = 500)
#'
#' @return a list of arguments matching [rstan::sampling()] inputs
#' @export
stan_options <- function(...) {
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
  int_args_in_sampling <- c("iter", "chains", "warmup", "cores")
  for (arg in intersect(names(res), int_args_in_sampling)) {
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
