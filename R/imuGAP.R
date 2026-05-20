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
  return(res)
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
  return(as.list(environment()))
}

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
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
#' @examples
#' \dontrun{
#' data("locations_sim"); data("observations_sim"); data("populations_sim")
#' st_opts <- stan_options(chains = 2, iter = 500)
#' imuGAP(
#'   observations_sim, populations_sim, locations_sim,
#'   stan_opts = st_opts
#' )
#' }
#'
#' @autoglobal
#' @export
sampling <- function( # nolint
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

  return(do.call(rstan::sampling, stan_opts))
}

#' @title Predict coverage probabilities
#'
#' @description
#' Uses the output of \code{\link{sampling}} and a target \code{populations} grid to generate
#' predicted coverage probabilities.
#'
#' @param fit a `stanfit` object returned by `sampling()`
#' @param populations a `[data.frame()]` of target populations to predict for
#' @param locations a `[data.frame()]` of the geographic hierarchy
#' @param imugap_opts options for the model, by default `imugap_options()`
#'
#' @return A `data.table` with columns `sample_id`, `obs_id`, and `p_obs` containing
#'   the predicted coverage probabilities for each posterior draw and target observation.
#'
#' @export
#' @importFrom data.table as.data.table copy data.table
#' @importFrom rstan gqs extract
predict <- function(
  fit,
  populations,
  locations,
  imugap_opts = imugap_options()
) {
  dims <- fit@par_dims
  if (is.null(dims)) {
    stop("Invalid 'fit' object. Ensure it is a valid stanfit object.", call. = FALSE)
  }

  if (!"beta_bs" %in% names(dims) || !"lambda_raw" %in% names(dims)) {
    stop("The 'fit' object does not appear to be an imuGAP model fit (missing 'beta_bs' or 'lambda_raw').", call. = FALSE)
  }

  # check locations
  loc_info <- canonicalize_locations(locations)
  layer_sizes <- loc_info[, .N, keyby = layer][, c(N)]

  # Check if model has counties and schools
  has_cnty <- "off_cnty" %in% names(dims)
  has_sch <- "off_sch" %in% names(dims)

  if (has_cnty) {
    if (length(layer_sizes) != 3) {
      stop(
        sprintf(
          "The fitted model has 3 layers (counties and schools), but the provided 'locations' has %d layers.",
          length(layer_sizes)
        ),
        call. = FALSE
      )
    }
    n_cnty <- layer_sizes[2]
    n_schl <- layer_sizes[3]

    n_cnty_fit <- dims$off_cnty
    n_sch_fit <- dims$off_sch

    if (n_cnty != n_cnty_fit) {
      stop(
        sprintf(
          "Number of counties in 'locations' (%d) does not match the fitted model (%d).",
          n_cnty, n_cnty_fit
        ),
        call. = FALSE
      )
    }
    if (n_schl != n_sch_fit) {
      stop(
        sprintf(
          "Number of schools in 'locations' (%d) does not match the fitted model (%d).",
          n_schl, n_sch_fit
        ),
        call. = FALSE
      )
    }
  } else {
    # Stateonly model
    if (length(layer_sizes) > 1) {
      stop("The fitted model is a stateonly model, but the provided 'locations' has counties/schools.", call. = FALSE)
    }
    n_cnty <- 0L
    n_schl <- 0L
  }

  populations <- data.table::copy(data.table::as.data.table(populations))

  # Ensure obs_id is present
  if (!"obs_id" %in% names(populations)) {
    populations[, obs_id := seq_len(.N)]
  }
  # Ensure weight is present
  if (!"weight" %in% names(populations)) {
    populations[, weight := 1.0]
  }

  # Generate dummy observations
  unique_obs_ids <- unique(populations$obs_id)
  obs <- data.table::data.table(
    obs_id = unique_obs_ids,
    positive = rep(0L, length(unique_obs_ids)),
    sample_n = rep(1L, length(unique_obs_ids)),
    censored = rep(NA_real_, length(unique_obs_ids))
  )
  obs <- canonicalize_observations(obs)

  # Determine max cohort and max age (n_yr) from populations
  max_cohort <- max(populations$cohort)
  max_age <- max(populations$age)

  # Check B-spline compatibility
  bsp <- splines::bs(
    seq_len(max_cohort),
    df = imugap_opts$df,
    intercept = TRUE
  )
  k_bs_fit <- dims$beta_bs
  if (ncol(bsp) != k_bs_fit) {
    stop(
      sprintf(
        "B-spline degrees of freedom / specification mismatch: target populations cohort range implies %d spline bases, but the fitted model has %d. Check df in imugap_opts.",
        ncol(bsp), k_bs_fit
      ),
      call. = FALSE
    )
  }

  # Check lambda size
  dose_schedule <- imugap_opts$dose_schedule
  n_doses_fit <- dims$lambda_raw
  if (length(dose_schedule) != n_doses_fit) {
    stop(
      sprintf(
        "Dose schedule mismatch: provided schedule has %d doses, but the fitted model has %d.",
        length(dose_schedule), n_doses_fit
      ),
      call. = FALSE
    )
  }

  wts <- canonicalize_populations(
    populations,
    obs,
    loc_info,
    max_cohort = max_cohort,
    max_age = max_age,
    max_dose = length(dose_schedule)
  )

  doses <- matrix(0, ncol = length(dose_schedule), nrow = max_age)
  for (i in seq_along(dose_schedule)) {
    doses[(dose_schedule[i] + 1):nrow(doses), i] <- 1
  }

  # prepare dat_stan for prediction mode
  dat_stan <- list(
    n_uncensored_obs = obs[is.na(censored), .N],
    n_yr = max_age,
    n_cohort = max_cohort,
    n_sch = if (has_sch) n_schl else 0L,
    n_doses = length(dose_schedule),
    dose_sched = doses,
    k_bs = ncol(bsp),
    bs = bsp,
    n_obs = nrow(obs),
    y_obs = obs$positive,
    y_smp = obs$sample_n,
    n_weights = nrow(wts),
    obs_to_weights_bounds = unique(wts$range_start),
    weights_cohort = wts$cohort,
    weights_life_year = wts$age,
    weights_dose = wts$dose,
    weights = wts$weight,
    predict_mode = 1
  )

  if (has_cnty) {
    dat_stan$n_cnty <- n_cnty
    dat_stan$cnty_bounds <- loc_info[layer == 3, unique(layer_bound)]
    dat_stan$weights_school <- wts$loc_c_id
  }

  # Run gqs to generate predictions
  gqs_res <- rstan::gqs(fit@stanmodel, data = dat_stan, draws = as.matrix(fit))

  # Extract predictions
  p_obs_draws <- rstan::extract(gqs_res, pars = "p_obs")$p_obs
  p_obs_draws <- as.matrix(p_obs_draws)

  # Match up with requested populations
  unique_obs <- unique(wts[, .(obs_c_id, obs_id)], by = "obs_c_id")
  n_draws <- nrow(p_obs_draws)
  n_obs <- ncol(p_obs_draws)

  res_dt <- data.table::data.table(
    sample_id = rep(seq_len(n_draws), times = n_obs),
    obs_id = rep(unique_obs$obs_id, each = n_draws),
    p_obs = as.vector(p_obs_draws)
  )

  return(res_dt)
}

#' @title Custom imuGAP fit extraction
#'
#' @description
#' Thin wrapper around `rstan::extract` to extract typical imuGAP parameters.
#' @param fit a `stanfit` object returned by `sampling()`
#' @param pars character vector; parameters to extract.
#' @param ... additional arguments passed to `[rstan::extract()]`.
#'
#' @return a list, as returned by `rstan::extract()`
#'
#' @examples
#' data("fit_sim")
#' extract_imugap(fit_sim)
#' extract_imugap(fit_sim, pars = "phi")
#'
#' @export
extract_imugap <- function(fit, pars = c("logit_phi_st"), ...) {
  return(rstan::extract(fit, pars = pars, ...))
}
