solver_type_map <- c(
  direct = 1L,
  halley2 = 2L,
  newton2 = 3L,
  builtin = 4L,
  halley10 = 5L
)

#' @title imuGAP Model Options
#'
#' @description
#' Configures model-side options for `imuGAP` estimation.
#'
#' @param df single positive integer; degrees of freedom to use for the cohort B-spline
#'   basis expansion (default: 5L).
#' @param dose_schedule ascending integer vector of ages at which each dose `1..n`
#'   becomes eligible (default: `c(1, 4)` for 2-dose vaccines).
#' @param compute_log_lik logical scalar; compute pointwise log-likelihood during
#'   sampling? (default: `FALSE`).
#' @param time character string specifying the temporal model component
#'   (default: `"default"`, resolving to `"bspline"`).
#' @param offsets character string specifying the spatial offset model component
#'   (default: `"default"`, resolving to `"static"`).
#' @param link character string specifying the link function formulation
#'   (default: `"default"`, resolving to `"logit"`). Options include `"default"` (or `"logit"`)
#'   and `"probit"`.
#' @param solver character string or integer specifying the rootfinder solver method
#'   (default: `"default"`, resolving to `"direct"`). Options include `"default"` (or `"direct"`),
#'   `"halley2"`, `"newton2"`, `"builtin"`, and `"halley10"`.
#' @param model optional character string; alias for `link` or full model template name
#'   (default: `NULL`).
#'
#' @examples
#' imugap_options()
#' imugap_options(dose_schedule = c(1, 3))
#' imugap_options(link = "probit", solver = "halley2")
#'
#' @return a named list, of `imuGAP` model options.
#' @export
imugap_options <- function(
  df = 5L,
  dose_schedule = c(1, 4),
  compute_log_lik = FALSE,
  time = c("default", "bspline"),
  offsets = c("default", "static"),
  link = c("default", "logit", "probit"),
  solver = c("default", "direct", "halley2", "newton2", "builtin", "halley10"),
  model = NULL
) {
  time <- match.arg(time)
  if (identical(time, "default")) {
    time <- "bspline"
  }
  offsets <- match.arg(offsets)
  if (identical(offsets, "default")) {
    offsets <- "static"
  }
  link <- match.arg(link)
  if (identical(link, "default")) {
    link <- "logit"
  }

  solver <- if (is.numeric(solver)) {
    stop_fmt_if(
      length(solver) != 1L ||
        is.na(solver) ||
        solver %% 1 != 0 ||
        !(as.integer(solver) %in% 1:5),
      ERR_OPT_UNKNOWN_SOLVER,
      solver = as.character(solver)
    )
    names(solver_type_map)[as.integer(solver)]
  } else {
    match.arg(solver)
  }
  if (identical(solver, "default")) {
    solver <- "direct"
  }

  if (!is.null(model)) {
    if (model %in% c("default", "logit", "probit")) {
      link <- if (identical(model, "default")) "logit" else model
    } else if (
      model %in%
        c(
          "bspline_static_logit",
          "bspline_static_probit",
          "bspline_static_offsets_logit",
          "bspline_static_offsets_probit"
        )
    ) {
      parts <- strsplit(model, "_", fixed = TRUE)[[1]]
      time <- parts[1]
      offsets <- "static"
      link <- parts[length(parts)]
    } else {
      stop_fmt_if(TRUE, ERR_OPT_UNKNOWN_MODEL, model = model)
    }
  }

  stop_fmt_if(length(df) != 1L, ERR_OPT_DF_SINGLE)
  df <- assert_positive_int(df, "df")

  dose_schedule <- assert_positive_int(dose_schedule, "dose_schedule")
  stop_fmt_if(
    is.unsorted(dose_schedule, strictly = TRUE),
    ERR_OPT_DOSE_SCHEDULE
  )

  stop_fmt_if(
    !is.logical(compute_log_lik) ||
      length(compute_log_lik) != 1L ||
      is.na(compute_log_lik),
    ERR_OPT_COMPUTE_LOG_LIK
  )

  model_template <- sprintf("%s_%s_%s", time, offsets, link)

  list(
    df = df,
    dose_schedule = dose_schedule,
    compute_log_lik = compute_log_lik,
    time = time,
    offsets = offsets,
    link = link,
    model = model_template,
    solver = solver,
    solver_type = unname(solver_type_map[solver])
  )
}
