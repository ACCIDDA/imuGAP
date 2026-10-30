solver_type_map <- c(
  direct = 1L,
  halley2 = 2L,
  newton2 = 3L,
  builtin = 4L,
  halley10 = 5L
)

guess_type_map <- c(
  zero = 1L,
  taylor2 = 2L,
  taylor4 = 3L,
  pade = 4L,
  asymptotic = 5L,
  conditioned = 6L
)


#' @title Resolve and assemble concrete Stan model name from options
#'
#' @description
#' Validates option compatibility and emits the concrete Stan model name along
#' with parsed model components.
#'
#' @param time character string; temporal component specification (default: `"bspline"`).
#'   Options include `"bspline"`.
#' @param offsets character string; spatial offset component specification
#'   (default: `"static"`). Options include `"static"`.
#' @param guesser character string; initial guesser method (default: `"zero"`).
#'   Options include `"zero"`.
#' @param solver character string; rootfinder solver method (default: `"halley2"`).
#'   Options include `"halley2"`, `"direct"`, `"newton2"`, `"builtin"`, and `"halley10"`.
#' @param link character string; link function formulation (default: `"logit"`).
#'   Options include `"logit"` and `"probit"`.
#'
#' @return a named list, containing:
#'   - `model_name`: character string of the concrete compiled Stan model.
#'   - `time`: character string temporal basis (`"bspline"`).
#'   - `offsets`: character string spatial offset type (`"static"`).
#'   - `guesser`: character string initial guesser method (`"zero"`).
#'   - `solver`: character string rootfinder method (`"halley2"`, `"direct"`, etc.).
#'   - `solver_module`: character string solver module (`"halley2"`, `"direct"`, or `"dispatch"`).
#'   - `solver_type`: integer solver code (1L..5L).
#'   - `link`: character string link formulation (`"logit"` or `"probit"`).
#'   - `is_dispatch`: logical scalar indicating whether dynamic solver dispatch is active.
#'
#' @examples
#' resolve_model_name()
#' resolve_model_name(solver = "newton2")
#' resolve_model_name(link = "probit")
#'
#' @keywords internal
#' @noRd
resolve_model_name <- function(
  time = c("bspline"),
  offsets = c("static"),
  guesser = c("zero"),
  solver = c("halley2", "direct", "newton2", "builtin", "halley10"),
  link = c("logit", "probit")
) {
  time <- match.arg(time)
  offsets <- match.arg(offsets)
  guesser <- match.arg(guesser)
  solver <- match.arg(solver)
  link <- match.arg(link)

  solver_type <- unname(solver_type_map[solver])

  if (identical(solver, "halley2")) {
    is_dispatch <- FALSE
    solver_module <- solver
    model_name <- sprintf("%s_static_%s_%s_%s", time, guesser, solver, link)
  } else {
    is_dispatch <- TRUE
    solver_module <- "dispatch"
    model_name <- sprintf("%s_static_%s_dispatch_%s", time, guesser, link)
  }

  list(
    model_name = model_name,
    time = time,
    offsets = offsets,
    guesser = guesser,
    solver = solver,
    solver_module = solver_module,
    solver_type = solver_type,
    link = link,
    is_dispatch = is_dispatch
  )
}

#' @title Resolve execution model name based on location hierarchy depth
#'
#' @description
#' Adapts a pre-validated `imugap_options` specification to the concrete location hierarchy
#' depth (single-layer vs. multi-layer). By naming convention, single-layer models are
#' `<time>_single_<link>`.
#'
#' @param imugap_opts an object of class `imugap_options`.
#' @param is_multilayer logical scalar; does the location hierarchy have two or more layers?
#'   (default: `TRUE`).
#'
#' @return a character string of the concrete compiled Stan model name.
#'
#' @keywords internal
#' @noRd
resolve_layer_model <- function(imugap_opts, is_multilayer = TRUE) {
  stop_fmt_if(!inherits(imugap_opts, "imugap_options"), ERR_IMUGAP_OPTS_CLASS)

  if (is_multilayer) {
    imugap_opts$model
  } else {
    sprintf("%s_single_%s", imugap_opts$time, imugap_opts$link)
  }
}

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
#'   (default: `"bspline"`). Options include `"bspline"`.
#' @param offsets character string specifying the spatial offset model component
#'   (default: `"static"`). Options include `"static"`.
#' @param link character string specifying the link function formulation
#'   (default: `"logit"`). Options include `"logit"` and `"probit"`.
#' @param solver character string specifying the rootfinder solver method
#'   (default: `"halley2"`). Options include `"halley2"`, `"direct"`, `"newton2"`,
#'   `"builtin"`, and `"halley10"`.
#' @examples
#' imugap_options()
#' imugap_options(dose_schedule = c(1, 3))
#' imugap_options(link = "probit", solver = "halley2")
#'
#' @return an object of class `imugap_options`, wrapping a named list of model options.
#' @export
imugap_options <- function(
  df = 5L,
  dose_schedule = c(1, 4),
  compute_log_lik = FALSE,
  time = c("bspline"),
  offsets = c("static"),
  link = c("logit", "probit"),
  solver = c("halley2", "direct", "newton2", "builtin", "halley10")
) {
  time <- match.arg(time)
  offsets <- match.arg(offsets)
  link <- match.arg(link)
  solver <- match.arg(solver)

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

  resolved <- resolve_model_name(
    time = time,
    offsets = offsets,
    solver = solver,
    link = link
  )

  structure(
    list(
      df = df,
      dose_schedule = dose_schedule,
      compute_log_lik = compute_log_lik,
      time = resolved$time,
      offsets = resolved$offsets,
      link = resolved$link,
      model = resolved$model_name,
      solver = resolved$solver,
      solver_type = resolved$solver_type
    ),
    class = "imugap_options"
  )
}
