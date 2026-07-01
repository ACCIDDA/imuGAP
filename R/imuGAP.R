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
  stan_opts$data <- list(
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
  stan_opts$object <- imugap_opts$object

  raw_stanfit <- do.call(rstan::sampling, stan_opts)

  structure(
    list(
      stanfit = raw_stanfit,
      settings = list(
        imugap_opts = imugap_opts,
        stan_opts = stan_opts
      ),
      data = stan_opts$data[setdiff(names(stan_opts$data), "object")],
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

#' @keywords internal
internal_target_builder_vec <- function(
  location,
  age,
  cohort,
  dose,
  mode
) {
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

#' @keywords internal
internal_target_builder_df <- function(location) {
  tmp <- data.table::as.data.table(location)
  checked_cols(tmp, c("loc_id", "age", "cohort", "dose"))

  if (!"obs_c_id" %in% names(tmp)) {
    tmp[, obs_c_id := seq_len(.N)]
  } else {
    if (!all(tmp$obs_c_id == seq_len(nrow(tmp)))) {
      stop("if supplied, obs_c_id must be 1:nrow(target)", call. = FALSE)
    }
  }

  if ("obs_id" %in% names(tmp)) {
    if (any(duplicated(tmp$obs_id)) || any(is.na(tmp$obs_id))) {
      stop("if supplied, obs_id must be unique and not NA", call. = FALSE)
    }
  }

  if (!"weight" %in% names(tmp)) {
    tmp[, weight := 1.0]
  } else {
    if (!all(tmp$weight == 1.0)) {
      stop("if supplied, weight must be 1", call. = FALSE)
    }
  }

  cols <- c("obs_c_id", "loc_id", "age", "cohort", "dose", "weight")
  if ("obs_id" %in% names(tmp)) {
    cols <- c(cols, "obs_id")
  }
  tmp[, .SD, .SDcols = cols]
}


#' @title Construct a target grid for prediction
#'
#' @description
#' Builds a target grid for use with `[predict.imugap_fit()]`. This is pure
#' construction and does **not** reference a fitted model, so it can be called
#' without a fit (e.g. to expand a request into rows before any fit exists). To
#' check a constructed target against a specific fit, use
#' `[canonicalize_target()]`; `[predict.imugap_fit()]` does this for you.
#'
#' @param location either a vector of locations or a `data.frame`; if a vector of
#'   locations, treated as the target locations (and `age`, `cohort`, `dose` must
#'   be supplied); if a `data.frame`, it is normalised as the target grid and
#'   `age`, `cohort`, and `dose` must not be supplied.
#' @param age vector of ages for which to predict coverage, consistent with
#'   `[canonicalize_populations()]`
#' @param cohort vector of cohorts for which to predict coverage, consistent with
#'   `[canonicalize_populations()]`
#' @param dose vector of doses for which to predict coverage, consistent with
#'   `[canonicalize_observations()]`
#' @param mode one of `"error"` (default), `"enumerate"`, `"recycle"`, or
#'   `"snapshot"`, controlling how vector inputs combine:
#'   \describe{
#'     \item{`"error"`}{all vector inputs must have the same length.}
#'     \item{`"enumerate"`}{all combinations of the inputs.}
#'     \item{`"recycle"`}{recycle inputs to the least-common-multiple length.}
#'     \item{`"snapshot"`}{`cohort` must be a single reference value (the oldest
#'       cohort); locations, ages, and doses are enumerated with a cohort for each
#'       age such that `age + cohort` is constant
#'       (`cohort_i = cohort_ref + max_age - age_i`), i.e. a snapshot in time.}
#'   }
#'   Ignored when `location` is a `data.frame`.
#'
#' @return a `data.table` target grid with columns `obs_c_id`, `loc_id`, `age`,
#'   `cohort`, `dose`, and `weight` (plus `obs_id` if supplied).
#'
#' @seealso `[canonicalize_target()]`, `[predict.imugap_fit()]`
#'
#' @examples
#' # 1. Default "error" mode: all vector inputs must have the same length.
#' create_target(
#'   location = c("Blue Heron School", "Bluebird Learning Center"),
#'   age = c(1, 2),
#'   cohort = c(2, 3),
#'   dose = c(1, 1),
#'   mode = "error"
#' )
#'
#' # Mismatched lengths in "error" mode throw an error:
#' try(create_target(
#'   location = c("Blue Heron School", "Bluebird Learning Center"),
#'   age = c(1),
#'   cohort = c(2, 3),
#'   dose = c(1, 1),
#'   mode = "error"
#' ))
#'
#' # 2. "enumerate" mode: all combinations of the arguments.
#' create_target(
#'   location = c("Blue Heron School", "Bluebird Learning Center"),
#'   age = c(1, 2),
#'   cohort = c(2, 3),
#'   dose = c(1),
#'   mode = "enumerate"
#' )
#'
#' # 3. "recycle" mode: recycle arguments to the least-common-multiple length.
#' create_target(
#'   location = c("Blue Heron School", "Bluebird Learning Center"),
#'   age = c(1, 2, 3),
#'   cohort = c(2),
#'   dose = c(1),
#'   mode = "recycle"
#' )
#'
#' # 4. "snapshot" mode: cohort is a single reference value; cohorts are set so
#' # that cohort + age is constant (a snapshot in time).
#' create_target(
#'   location = c("Blue Heron School", "Bluebird Learning Center"),
#'   age = c(1, 2, 3),
#'   cohort = 5,
#'   dose = c(1),
#'   mode = "snapshot"
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

  # check if location is a data.frame or vector
  if (!is.data.frame(location)) {
    internal_target_builder_vec(location, age, cohort, dose, mode)
  } else {
    if (!missing(age) || !missing(cohort) || !missing(dose)) {
      stop(
        "age, cohort, and dose must not be supplied when location is a data.frame",
        call. = FALSE
      )
    }
    internal_target_builder_df(location)
  }
}

#' @title Validate a target grid against a fit
#'
#' @description
#' Checks a constructed target (see `[create_target()]`) against a specific
#' `imugap_fit`: every `loc_id` must exist in the fit, and `dose`, `age`, and
#' `cohort` must be within the fit's ranges. The canonical `loc_c_id` is added
#' from the fit. Errors on any out-of-range value. `[predict.imugap_fit()]` calls
#' this internally, so most users do not call it directly.
#'
#' @param fit an `imugap_fit` object returned by `[sampling()]`
#' @param target a target grid, e.g. the output of `[create_target()]`
#'
#' @return the validated `target` (a `data.table`) with `loc_c_id` added.
#'
#' @seealso `[create_target()]`, `[predict.imugap_fit()]`
#'
#' @examples
#' data("fit_sim")
#' target <- create_target(
#'   location = c("Blue Heron School", "Bluebird Learning Center"),
#'   age = c(1, 2, 3),
#'   cohort = 5,
#'   dose = c(1),
#'   mode = "snapshot"
#' )
#' canonicalize_target(fit_sim, target)
#'
#' @importFrom data.table as.data.table between
#' @autoglobal
#' @export
canonicalize_target <- function(fit, target) {
  target <- data.table::as.data.table(target)

  #  - check that all locations are within fit$locations
  invalid_locs <- setdiff(target$loc_id, fit$locations$loc_id)
  if (length(invalid_locs) > 0) {
    stop(
      "all locations must be within fit$locations. Invalid locations: ",
      toString(invalid_locs, width = 60),
      call. = FALSE
    )
  }

  #  - check dose are within fit$data$n_doses
  invalid_dose_rows <- target[, which(!between(dose, 1L, fit$data$n_doses))]
  if (length(invalid_dose_rows) > 0) {
    stop(
      sprintf(
        "dose values must be within 1 and fit$data$n_doses (%i). Invalid dose in rows: ",
        fit$data$n_doses
      ),
      toString(invalid_dose_rows, width = 60),
      call. = FALSE
    )
  }

  #  - check age is within fit$data$n_yr
  invalid_age_rows <- target[, which(!between(age, 1L, fit$data$n_yr))]
  if (length(invalid_age_rows) > 0) {
    stop(
      sprintf(
        "age values must be within 1 and fit$data$n_yr (%i). Invalid age in rows: ",
        fit$data$n_yr
      ),
      toString(invalid_age_rows, width = 60),
      call. = FALSE
    )
  }

  #  - check cohort is within fit$data$n_cohort
  invalid_cohort_rows <- target[, which(
    !between(cohort, 1L, fit$data$n_cohort)
  )]
  if (length(invalid_cohort_rows) > 0) {
    stop(
      sprintf(
        "cohort values must be within 1 and fit$data$n_cohort (%i). Invalid cohort in rows: ",
        fit$data$n_cohort
      ),
      toString(invalid_cohort_rows, width = 60),
      call. = FALSE
    )
  }

  # use fit$locations to add corresponding loc_c_id to target
  target[fit$locations, on = .(loc_id), loc_c_id := loc_c_id]

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
  rstan::extract(fit$stanfit, pars = pars, ...)
}
