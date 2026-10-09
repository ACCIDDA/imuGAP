guess_type_map <- c(
  default = 3L,
  taylor4 = 3L,
  zero = 1L,
  taylor2 = 2L,
  pade = 4L,
  asymptotic = 5L,
  conditioned = 6L
)

solver_type_map <- c(
  default = 1L,
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
#' @param link character string specifying the link function formulation (default: `"default"`).
#'   Options include `"default"` (or `"logit"`) for the logit link and `"probit"` for the
#'   probit link. Dispatch to optimized single versus multi-layer versions occurs
#'   automatically within `[sampling()]`.
#' @param guess character string specifying the initial shift guess method (default: `"default"`).
#'   Options include `"default"` (alias for `"taylor4"`), `"taylor4"`, `"zero"`, `"taylor2"`,
#'   `"pade"`, `"asymptotic"`, and `"conditioned"`.
#' @param solver character string specifying the rootfinder solver method (default: `"default"`).
#'   Options include `"default"` (alias for `"direct"`), `"direct"`, `"halley2"`, `"newton2"`,
#'   `"builtin"`, and `"halley10"`.
#' @param model deprecated alias for `link`.
#'
#' @examples
#' imugap_options()
#' imugap_options(dose_schedule = c(1, 3))
#' imugap_options(link = "probit", guess = "taylor4", solver = "halley2")
#'
#' @return a named list, of `imuGAP` model options.
#' @export
imugap_options <- function(
  df = 5L,
  dose_schedule = c(1, 4),
  compute_log_lik = FALSE,
  link = c("default", "logit", "probit"),
  guess = c(
    "default",
    "taylor4",
    "zero",
    "taylor2",
    "pade",
    "asymptotic",
    "conditioned"
  ),
  solver = c("default", "direct", "halley2", "newton2", "builtin", "halley10"),
  model = link
) {
  if (!missing(model) && missing(link)) {
    link <- model
  }
  link <- match.arg(link, c("default", "logit", "probit"))
  guess <- if (
    is.numeric(guess) &&
      length(guess) == 1L &&
      !is.na(guess) &&
      guess %% 1 == 0 &&
      as.integer(guess) %in% 1:6
  ) {
    c(
      "zero",
      "taylor2",
      "taylor4",
      "pade",
      "asymptotic",
      "conditioned"
    )[as.integer(guess)]
  } else {
    match.arg(guess)
  }
  if (identical(guess, "default")) {
    guess <- "taylor4"
  }

  solver <- if (
    is.numeric(solver) &&
      length(solver) == 1L &&
      !is.na(solver) &&
      solver %% 1 == 0 &&
      as.integer(solver) %in% 1:5
  ) {
    c("direct", "halley2", "newton2", "builtin", "halley10")[as.integer(solver)]
  } else {
    match.arg(solver)
  }
  if (identical(solver, "default")) {
    solver <- "direct"
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

  list(
    df = df,
    dose_schedule = dose_schedule,
    compute_log_lik = compute_log_lik,
    link = link,
    model = link,
    guess = guess,
    guess_type = unname(guess_type_map[guess]),
    solver = solver,
    solver_type = unname(solver_type_map[solver])
  )
}
