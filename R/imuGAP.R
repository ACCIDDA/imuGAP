#' @title Immunity: Geographic & Age-based Projection, `imuGAP`
#'
#' @description
#' This a sampler interface to convert user-friendly data into the necessary
#' format to feed the immunity estimation model.
#'
#' @inheritParams canonicalize_observations
#' @inheritParams canonicalize_populations
#' @inheritParams canonicalize_locations
#' @param imugap_opts options for the `imuGAP` model
#' @param stan_opts passed to `rstan::sampling` (e.g. `iter`, `chains`).
#'
#' @return An object of class `imugap_fit` wrapping the raw `stanfit` object
#'   along with settings and dataset metadata.
#'
#' @details
#' If the Stan sampler fails to initialize and produces no draws (for the rstan
#' backend, a mode-2 `stanfit` with an empty `@sim`), `sampling()` raises an
#' error of class `imugap_no_draws` rather than returning an empty fit, so the
#' failure can be handled with `tryCatch()`. The check is backend-agnostic (see
#' `backend_has_draws()`).
#'
#' @examples
#' \donttest{
#' data("locations_sim"); data("observations_sim"); data("populations_sim")
#' st_opts <- stan_options(chains = 2, iter = 500)
#' sampling(
#'   observations_sim, populations_sim, locations_sim,
#'   stan_opts = st_opts
#' )
#' }
#'
#' @autoglobal
#' @export
sampling <- function(
  observations,
  populations,
  locations,
  imugap_opts = imugap_options(),
  stan_opts = stan_options()
) {
  # check location argument
  loc_info <- canonicalize_locations(locations)
  layer_sizes <- loc_info[, .N, keyby = layer][, c(N)]
  if (length(layer_sizes) != 3) {
    stop(
      "imuGAP currently only supports 3-layer models ",
      "(e.g. single state => counties => schools); offered ",
      length(layer_sizes),
      " layers."
    )
  }

  n_cnty <- layer_sizes[2]
  n_schl <- layer_sizes[3]

  # check observations argument
  obs <- canonicalize_observations(observations)

  # check populations - confirm wts locations
  wts <- canonicalize_populations(
    populations,
    obs,
    loc_info
  )

  bsp <- splines::bs(
    seq_len(wts[, diff(range(cohort)) + 1L]),
    df = imugap_opts$df,
    intercept = TRUE
  )

  dose_schedule <- imugap_opts$dose_schedule

  doses <- matrix(0, ncol = length(dose_schedule), nrow = max(wts$age))
  for (i in seq_along(dose_schedule)) {
    doses[(dose_schedule[i] + 1):nrow(doses), i] <- 1
  }

  # prepare dat_stan
  dat_stan <- list(
    n_uncensored_obs = obs[is.na(censored), .N],
    n_yr = max(wts$age),
    n_cohort = max(wts$cohort),
    n_sch = n_schl,
    n_doses = length(dose_schedule),
    dose_sched = doses,
    k_bs = ncol(bsp),
    bs = bsp,
    n_obs = nrow(obs),
    y_obs = obs$positive,
    y_smp = obs$sample_n,
    n_weights = nrow(wts),
    obs_to_weights_bounds = unique(wts$range_start),
    weights_school = wts$loc_c_id,
    weights_cohort = wts$cohort,
    weights_life_year = wts$age,
    weights_dose = wts$dose,
    weights = wts$weight,
    n_cnty = n_cnty,
    cnty_bounds = loc_info[layer == 3, unique(layer_bound)],
    predict_mode = 0
  )

  # The `backend` element is the marker that stan_opts came from stan_options();
  # its absence means a hand-built list. Whatever backend it names wins.
  backend <- stan_opts$backend
  if (is.null(backend)) {
    stop("`stan_opts` must be created by stan_options().", call. = FALSE)
  }

  # imuGAP has a single model; fit_model() looks it up in `stanmodels` (rstan)
  # and locates inst/stan/<model_name>.stan (cmdstanr). No init is supplied, so
  # the backend uses its own default.
  model_name <- "impute_school_coverage_process_v6"
  raw_fit <- fit_model(
    model_name, dat_stan, init = NULL, stan_opts, drop_pars = NULL
  )

  # fit_model() dispatches to the active backend, so the fit may be an rstan
  # stanfit or a cmdstanr CmdStanMCMC. A sampler that fails to initialize can
  # return an empty fit rather than erroring (rstan does this with a mode-2
  # stanfit); passing it through would let a caller using tryCatch(error=)
  # mistake the failure for success. Detect the no-draws case in a
  # backend-agnostic way and raise a typed error instead. (#107)
  if (!backend_has_draws(raw_fit)) {
    stop(errorCondition(
      paste0(
        "the Stan sampler produced no draws; it most likely failed to ",
        "initialize on a data or dimension problem, so no usable fit was ",
        "produced."
      ),
      class = "imugap_no_draws"
    ))
  }

  structure(
    list(
      stanfit = raw_fit,
      settings = list(
        imugap_opts = imugap_opts,
        stan_opts = stan_opts
      ),
      data = dat_stan,
      locations = loc_info
    ),
    class = "imugap_fit"
  )
}

#' @keywords internal
gcd <- function(a, b) {
  while (b != 0) {
    temp <- b
    b <- a %% b
    a <- temp
  }
  a
}

#' @keywords internal
lcm <- function(a, b) {
  (a * b) / gcd(a, b)
}

#' @keywords internal
compute_recycled_target_len <- function(lens) {
  target_len <- lens[1]
  for (len in lens[-1L]) {
    target_len <- lcm(target_len, len)
  }
  target_len
}

#' @keywords internal
validate_vec_inputs <- function(location, age, cohort, dose) {
  if (missing(age) || missing(cohort) || missing(dose)) {
    stop(
      "age, cohort, and dose must be supplied when location is a vector",
      call. = FALSE
    )
  }

  na_args <- c("location", "age", "cohort", "dose")[which(
    c(
      any(is.na(location)),
      any(is.na(age)),
      any(is.na(cohort)),
      any(is.na(dose))
    )
  )]

  if (length(na_args) > 0) {
    stop(
      "No arguments may have NA values; the following do: ",
      toString(na_args),
      call. = FALSE
    )
  }

  n_loc <- length(location)
  n_age <- length(age)
  n_coh <- length(cohort)
  n_dos <- length(dose)

  zero_lens <- c("location", "age", "cohort", "dose")[which(
    c(n_loc, n_age, n_coh, n_dos) == 0L
  )]

  if (length(zero_lens) > 0) {
    stop(
      "No arguments may have length zero; the following do: ",
      toString(zero_lens),
      call. = FALSE
    )
  }
  c(n_loc = n_loc, n_age = n_age, n_coh = n_coh, n_dos = n_dos)
}

#' @title Construct a target grid for prediction
#'
#' @description
#' Builds a target grid, for use with `[predict.imugap_fit()]`, from vectors of
#' locations, ages, cohorts, and doses. This is pure construction and does not
#' reference a fitted model, so it can be called without a fit (e.g. to expand a
#' request into rows before any fit exists). To validate a target against a
#' specific fit -- or to canonicalize a target you built yourself as a
#' `data.frame` -- use `[canonicalize_target()]`; `[predict.imugap_fit()]` does
#' this for you.
#'
#' @param location a vector of location IDs to target.
#' @param age vector of ages for which to predict coverage, consistent with
#'   `[canonicalize_populations()]`.
#' @param cohort vector of cohorts for which to predict coverage, consistent with
#'   `[canonicalize_populations()]`.
#' @param dose vector of doses for which to predict coverage, consistent with
#'   `[canonicalize_observations()]`.
#' @param mode one of `"error"` (default), `"enumerate"`, `"recycle"`, or
#'   `"snapshot"`, controlling how the vector inputs combine:
#'
#'   - `"error"`: all vector inputs must have the same length.
#'   - `"enumerate"`: all combinations of the inputs.
#'   - `"recycle"`: recycle the inputs out to the least-common-multiple length.
#'   - `"snapshot"`: `cohort` must be a single reference value (the oldest
#'     cohort); locations, ages, and doses are enumerated with a cohort for each
#'     age such that `age + cohort` is constant, using the **maximum** value of
#'     `age` to set that constant (`cohort_i = cohort_ref + max(age) - age_i`),
#'     i.e. a snapshot in time.
#'
#' @return a `data.table` target grid with columns `obs_c_id`, `loc_id`, `age`,
#'   `cohort`, `dose`, and `weight`.
#'
#' @seealso `[canonicalize_target()]`, `[predict.imugap_fit()]`
#'
#' @examples
#' # "error" mode: all vector inputs must have the same length.
#' create_target(
#'   location = c("Blue Heron School", "Bluebird Learning Center"),
#'   age = c(1, 2), cohort = c(2, 3), dose = c(1, 1), mode = "error"
#' )
#'
#' # "enumerate": all combinations of the inputs.
#' create_target(
#'   location = c("Blue Heron School", "Bluebird Learning Center"),
#'   age = c(1, 2), cohort = c(2, 3), dose = c(1), mode = "enumerate"
#' )
#'
#' # "snapshot": cohort is a single reference; cohorts are set so age + cohort is
#' # constant, using max(age).
#' create_target(
#'   location = c("Blue Heron School", "Bluebird Learning Center"),
#'   age = c(1, 2, 3), cohort = 5, dose = c(1), mode = "snapshot"
#' )
#'
#' @importFrom data.table as.data.table copy data.table
#' @export
create_target <- function(
  location,
  age,
  cohort,
  dose,
  mode = c("error", "enumerate", "recycle", "snapshot")
) {
  mode <- match.arg(mode)
  lens <- validate_vec_inputs(location, age, cohort, dose)

  if (mode == "error") {
    if (length(unique(lens)) > 1L) {
      stop(
        "All arguments must have the same length in 'error' mode",
        call. = FALSE
      )
    }
    target <- data.table::data.table(
      loc_id = location,
      age = age,
      cohort = cohort,
      dose = dose,
      weight = 1.0
    )
  } else if (mode %in% c("enumerate", "snapshot")) {
    if (mode == "snapshot" && length(cohort) != 1L) {
      stop(
        "cohort must be a single reference value in 'snapshot' mode",
        call. = FALSE
      )
    }
    target <- data.table::as.data.table(expand.grid(
      loc_id = location,
      age = age,
      cohort = cohort,
      dose = dose,
      weight = 1.0,
      stringsAsFactors = FALSE
    ))

    if (mode == "snapshot") {
      ref_cohort <- cohort
      max_age <- max(age)
      target[, cohort := ref_cohort + max_age - age]
    }
  } else if (mode == "recycle") {
    target_len <- compute_recycled_target_len(lens)

    target <- data.table::data.table(
      loc_id = rep_len(location, target_len),
      age = rep_len(age, target_len),
      cohort = rep_len(cohort, target_len),
      dose = rep_len(dose, target_len),
      weight = 1.0
    )
  }
  target[, obs_c_id := seq_len(.N)]
  data.table::setcolorder(
    target,
    c("obs_c_id", "loc_id", "age", "cohort", "dose", "weight")
  )
  target[]
}

#' @title Custom imuGAP fit extraction
#'
#' @description
#' Thin wrapper around `rstan::extract` to extract typical imuGAP parameters.
#' @param fit an `imugap_fit` object returned by `sampling()`
#' @param pars character vector; parameters to extract. Defaults to
#'   `"beta_bs"`, the state-level B-spline parameter.
#' @param ... additional arguments passed to `[rstan::extract()]`.
#'
#' @return a list, as returned by `rstan::extract()`
#'
#' @examples
#' data("fit_sim")
#' extract_imugap(fit_sim)
#' extract_imugap(fit_sim, pars = "lambda_raw")
#'
#' @export
extract_imugap <- function(fit, pars = c("beta_bs"), ...) {
  if (!inherits(fit, "imugap_fit")) {
    stop("fit must be an object of class 'imugap_fit'", call. = FALSE)
  }
  # Extraction goes through the backend accessor, which only implements the
  # rstan path today; cmdstanr fits expose draws differently, so fail clearly
  # here rather than deep inside the accessor.
  if (!inherits(fit$stanfit, "stanfit")) {
    stop(
      "extract_imugap() currently supports only the rstan backend. Refit with ",
      "stan_options(backend = 'rstan').",
      call. = FALSE
    )
  }
  backend_extract(fit$stanfit, pars = pars, ...)
}
