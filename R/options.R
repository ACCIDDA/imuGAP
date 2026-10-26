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

#' @title Resolve and validate concrete Stan model name from options
#'
#' @description
#' Converts `"default"` option tokens to concrete specifications, validates option
#' compatibility, handles single-layer versus multi-layer hierarchy conversions, and emits
#' the concrete Stan model name along with parsed model components.
#'
#' @param imugap_opts a list of imuGAP options returned by [imugap_options()], or
#'   a model template string or character vector (default: `NULL`).
#' @param time optional character string; temporal component specification (default: `NULL`).
#' @param offsets optional character string; spatial offset component specification
#'   (default: `NULL`).
#' @param guesser optional character string; initial guesser method (default: `NULL`).
#' @param solver optional character string or integer; rootfinder solver method (default: `NULL`).
#' @param link optional character string; link function formulation (default: `NULL`).
#' @param model optional character string; model template alias (default: `NULL`).
#' @param is_multilayer logical scalar; does the location hierarchy have two or more layers?
#'   (default: `TRUE`).
#'
#' @return a named list, containing:
#'   - `model_name`: character string of the concrete compiled Stan model.
#'   - `time`: character string temporal basis (e.g. `"bspline"`).
#'   - `offsets`: character string spatial offset type (`"static"` or `"single"`).
#'   - `guesser`: character string initial guesser method (`"zero"`).
#'   - `solver`: character string rootfinder method (`"halley2"`, `"direct"`, etc.).
#'   - `solver_module`: character string solver module (`"halley2"`, `"direct"`, `"dispatch"`, or
#'     `NULL`).
#'   - `solver_type`: integer solver code (1L..5L) or `NULL` if direct or single layer.
#'   - `link`: character string link formulation (`"logit"` or `"probit"`).
#'   - `is_multilayer`: logical scalar indicating whether hierarchy is multi-layer.
#'   - `is_dispatch`: logical scalar indicating whether dynamic solver dispatch is active.
#'
#' @examples
#' resolve_model_name()
#' resolve_model_name(is_multilayer = FALSE)
#' resolve_model_name(imugap_options(solver = "newton2"))
#' resolve_model_name("default_default_default_default_probit")
#' resolve_model_name(c("default", "default", "default", "default", "probit"))
#'
#' @keywords internal
#' @noRd
resolve_model_name <- function(
  imugap_opts = NULL,
  time = NULL,
  offsets = NULL,
  guesser = NULL,
  solver = NULL,
  link = NULL,
  model = NULL,
  is_multilayer = TRUE
) {
  raw_model <- model
  solver_type <- NULL

  if (is.character(imugap_opts)) {
    if (length(imugap_opts) > 1L) {
      imugap_opts <- paste(imugap_opts, collapse = "_")
    }
    if (is.null(raw_model)) {
      raw_model <- imugap_opts
    }
  } else if (is.list(imugap_opts)) {
    raw_model <- raw_model %||% imugap_opts$model
    time <- time %||% imugap_opts$time
    offsets <- offsets %||% imugap_opts$offsets
    guesser <- guesser %||% imugap_opts$guesser
    solver <- solver %||% imugap_opts$solver
    solver_type <- imugap_opts$solver_type
    link <- link %||% imugap_opts$link
  }

  time <- time %||% "default"
  offsets <- offsets %||% "default"
  guesser <- guesser %||% "default"
  solver <- solver %||% "default"
  link <- link %||% "default"

  if (!is.null(raw_model)) {
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
        if (identical(parts[3L], "single")) {
          offsets <- "single"
        } else if (
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
        if (identical(parts[2L], "single")) {
          offsets <- "single"
        } else if (!identical(parts[2L], "default")) {
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
  if (identical(link, "default")) {
    link <- "logit"
  }
  if (identical(guesser, "default")) {
    guesser <- "zero"
  }
  if (identical(solver, "default")) {
    solver <- "halley2"
  }

  # Convert offsets based on layer hierarchy depth
  if (!is_multilayer || identical(offsets, "single")) {
    offsets <- "single"
    is_multilayer <- FALSE
  } else if (identical(offsets, "default")) {
    offsets <- "static"
  }

  stop_fmt_if(
    !time %in% c("bspline"),
    ERR_OPT_UNKNOWN_MODEL,
    model = raw_model %||% time
  )
  stop_fmt_if(
    !link %in% c("logit", "probit"),
    ERR_OPT_UNKNOWN_MODEL,
    model = raw_model %||% link
  )
  stop_fmt_if(
    !offsets %in% c("static", "single"),
    ERR_OPT_UNKNOWN_MODEL,
    model = raw_model %||% offsets
  )
  stop_fmt_if(
    !guesser %in% c("zero"),
    ERR_OPT_UNKNOWN_MODEL,
    model = raw_model %||% guesser
  )

  if (is.numeric(solver)) {
    stop_fmt_if(
      length(solver) != 1L ||
        is.na(solver) ||
        solver %% 1 != 0 ||
        !(as.integer(solver) %in% 1:5),
      ERR_OPT_UNKNOWN_SOLVER,
      solver = as.character(solver)
    )
    solver <- names(solver_type_map)[as.integer(solver)]
  }
  stop_fmt_if(
    !solver %in%
      c("direct", "dispatch", "halley2", "newton2", "builtin", "halley10"),
    ERR_OPT_UNKNOWN_SOLVER,
    solver = solver
  )

  if (identical(offsets, "single")) {
    solver_module <- NULL
    is_dispatch <- FALSE
    solver_type <- NULL
    model_name <- sprintf("%s_single_%s", time, link)
  } else if (identical(solver, "direct")) {
    is_dispatch <- FALSE
    solver_module <- solver
    solver_type <- NULL
    model_name <- sprintf("%s_static_%s_%s_%s", time, guesser, solver, link)
  } else {
    is_dispatch <- TRUE
    solver_module <- "dispatch"
    solver_type <- solver_type %||% unname(solver_type_map[solver])
    if (is.na(solver_type)) {
      solver_type <- 2L
    }
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
    is_multilayer = is_multilayer,
    is_dispatch = is_dispatch
  )
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
#'   (default: `"default"`, resolving to `"static"`). Options include `"default"`, `"static"`,
#'   and `"single"`.
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
  offsets = c("default", "static", "single"),
  link = c("default", "logit", "probit"),
  solver = c("default", "direct", "halley2", "newton2", "builtin", "halley10"),
  model = NULL
) {
  time <- match.arg(time)
  offsets <- match.arg(offsets)
  link <- match.arg(link)
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
    list(
      time = time,
      offsets = offsets,
      solver = solver,
      link = link,
      model = model
    ),
    is_multilayer = !identical(offsets, "single")
  )

  list(
    df = df,
    dose_schedule = dose_schedule,
    compute_log_lik = compute_log_lik,
    time = resolved$time,
    offsets = resolved$offsets,
    link = resolved$link,
    model = resolved$model_name,
    solver = resolved$solver,
    solver_type = resolved$solver_type %||%
      unname(solver_type_map[resolved$solver])
  )
}
