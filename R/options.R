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

supported_models <- c(
  "bspline_static_zero_dispatch_logit",
  "bspline_static_zero_dispatch_probit",
  "bspline_static_zero_direct_logit",
  "bspline_static_zero_direct_probit",
  "bspline_single_logit",
  "bspline_single_probit"
)

#' @title Resolve and assemble concrete Stan model name from options
#'
#' @description
#' Converts `"default"` option tokens to concrete specifications, validates option
#' compatibility, and emits the concrete Stan model name along with parsed model components.
#'
#' @param time character string; temporal component specification (default: `"default"`,
#'   resolving to `"bspline"`).
#' @param offsets character string; spatial offset component specification
#'   (default: `"default"`, resolving to `"static"`).
#' @param guesser character string; initial guesser method (default: `"default"`,
#'   resolving to `"zero"`).
#' @param solver character string; rootfinder solver method (default: `"default"`,
#'   resolving to `"direct"`).
#' @param link character string; link function formulation (default: `"default"`,
#'   resolving to `"logit"`).
#' @param model optional character string or vector; model template alias (default: `NULL`).
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
#' resolve_model_name(model = "default_default_default_default_probit")
#' resolve_model_name(model = c("default", "default", "default", "default", "probit"))
#'
#' @keywords internal
#' @noRd
resolve_model_name <- function(
  time = "default",
  offsets = "default",
  guesser = "default",
  solver = "default",
  link = "default",
  model = NULL
) {
  raw_model <- model
  solver_type <- NULL

  if (!is.null(raw_model)) {
    if (length(raw_model) > 1L) {
      raw_model <- paste(raw_model, collapse = "_")
    }
    if (raw_model %in% c("default", "logit", "probit")) {
      link <- if (identical(raw_model, "default")) "logit" else raw_model
    } else {
      parts <- strsplit(raw_model, "_", fixed = TRUE)[[1L]]
      if (length(parts) == 5L) {
        if (!identical(parts[1L], "default")) {
          time <- parts[1L]
        }
        if (!identical(parts[2L], "default")) {
          offsets <- parts[2L]
        }
        if (!identical(parts[3L], "default")) {
          guesser <- parts[3L]
        }
        if (
          !identical(parts[4L], "default") &&
            (identical(solver, "default") || identical(solver, "dispatch"))
        ) {
          solver <- parts[4L]
        }
        if (!identical(parts[5L], "default")) link <- parts[5L]
      } else if (length(parts) == 4L) {
        if (!identical(parts[1L], "default")) {
          time <- parts[1L]
        }
        if (!identical(parts[2L], "default")) {
          offsets <- parts[2L]
        }
        if (
          !identical(parts[3L], "default") &&
            (identical(solver, "default") || identical(solver, "dispatch"))
        ) {
          solver <- parts[3L]
        }
        if (!identical(parts[4L], "default")) link <- parts[4L]
      } else if (length(parts) == 3L) {
        if (!identical(parts[1L], "default")) {
          time <- parts[1L]
        }
        if (!identical(parts[2L], "default")) {
          offsets <- parts[2L]
        }
        if (!identical(parts[3L], "default")) link <- parts[3L]
      } else if (length(parts) == 1L) {
        link <- parts[1L]
      } else {
        stop_fmt_if(TRUE, ERR_OPT_UNKNOWN_MODEL, model = raw_model)
      }
    }
  }

  if (identical(time, "default")) {
    time <- "bspline"
  }
  if (identical(offsets, "default")) {
    offsets <- "static"
  }
  if (identical(link, "default")) {
    link <- "logit"
  }
  if (identical(guesser, "default")) {
    guesser <- "zero"
  }
  if (identical(solver, "default")) {
    solver <- "direct"
  }

  stop_fmt_if(
    !time %in% c("bspline"),
    ERR_OPT_UNKNOWN_MODEL,
    model = if (!is.null(raw_model)) raw_model else time
  )
  stop_fmt_if(
    !link %in% c("logit", "probit"),
    ERR_OPT_UNKNOWN_MODEL,
    model = if (!is.null(raw_model)) raw_model else link
  )
  stop_fmt_if(
    !offsets %in% c("static"),
    ERR_OPT_UNKNOWN_MODEL,
    model = if (!is.null(raw_model)) raw_model else offsets
  )
  stop_fmt_if(
    !guesser %in% c("zero"),
    ERR_OPT_UNKNOWN_MODEL,
    model = if (!is.null(raw_model)) raw_model else guesser
  )

  stop_fmt_if(
    !solver %in%
      c("direct", "dispatch", "halley2", "newton2", "builtin", "halley10"),
    ERR_OPT_UNKNOWN_SOLVER,
    solver = solver
  )

  if (is.null(solver_type)) {
    solver_type <- unname(solver_type_map[solver])
  }
  if (is.na(solver_type)) {
    solver_type <- 2L
  }

  if (identical(solver, "direct")) {
    is_dispatch <- FALSE
    solver_module <- solver
    model_name <- sprintf("%s_static_%s_%s_%s", time, guesser, solver, link)
  } else {
    is_dispatch <- TRUE
    solver_module <- "dispatch"
    model_name <- sprintf("%s_static_%s_dispatch_%s", time, guesser, link)
  }

  stop_fmt_if(
    !model_name %in% supported_models,
    ERR_OPT_UNKNOWN_MODEL,
    model = model_name
  )

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
#'   (default: `"default"`, resolving to `"bspline"`).
#' @param offsets character string specifying the spatial offset model component
#'   (default: `"default"`, resolving to `"static"`). Options include `"default"` and `"static"`.
#' @param link character string specifying the link function formulation
#'   (default: `"default"`, resolving to `"logit"`). Options include `"default"` (or `"logit"`)
#'   and `"probit"`.
#' @param solver character string specifying the rootfinder solver method
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
#' @return an object of class `imugap_options`, wrapping a named list of model options.
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
    link = link,
    model = model
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
