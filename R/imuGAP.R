# Internal error message format strings for imuGAP.R
ERR_STAN_OPTS_CLASS <- "`stan_opts` must be created by stan_options()"
ERR_EXTRACT_RSTAN_ONLY <- paste0(
  "extract_imugap() currently supports only the 'rstan' backend; ",
  "refit with stan_options(backend = 'rstan')"
)
ERR_DOSE_SCHEDULE_EMPTY <- "`dose_schedule` must not be empty"
ERR_DOSE_SCHED_OOB <- "`dose_schedule` contains no changepoints within 1..%d"
ERR_OPT_UNKNOWN_MODEL <- "`imugap_opts` unknown model '%s'"

#' @title Build sparse interval evaluation schedule
#'
#' @param dose_schedule Integer vector of dose eligibility changepoints.
#' @param ages Integer vector of observed ages.
#' @param max_age Single integer, maximum age considered.
#'
#' @return A named list containing `n_intervals`, `dt_vec`, `dose_sched`, and
#'   `age_to_interval_map`.
#'
#' @keywords internal
#' @noRd
build_interval_schedule <- function(dose_schedule, ages, max_age) {
  stop_fmt_if(length(dose_schedule) == 0L, ERR_DOSE_SCHEDULE_EMPTY)
  valid_sched <- dose_schedule[dose_schedule >= 1L & dose_schedule <= max_age]
  stop_fmt_if(
    length(valid_sched) == 0L,
    ERR_DOSE_SCHED_OOB,
    max_age
  )
  valid_ages <- ages[ages >= 1L & ages <= max_age]
  eval_ages <- sort(unique(c(valid_sched, valid_ages, max_age)))
  t_points <- c(0L, eval_ages)
  dt_vec <- as.numeric(diff(t_points))
  n_intervals <- length(eval_ages)

  dose_sched_mat <- matrix(
    0.0,
    nrow = n_intervals,
    ncol = length(dose_schedule)
  )
  for (k in seq_along(dose_schedule)) {
    dose_sched_mat[, k] <- as.numeric(eval_ages > dose_schedule[k])
  }

  matched <- match(seq_len(max_age), eval_ages)
  if (anyNA(matched)) {
    idx <- findInterval(seq_len(max_age), t_points, left.open = TRUE)
    idx[idx < 1L] <- 1L
    idx[idx > n_intervals] <- n_intervals
    matched[is.na(matched)] <- idx[is.na(matched)]
  }

  list(
    n_intervals = n_intervals,
    dt_vec = dt_vec,
    dose_sched = dose_sched_mat,
    age_to_interval_map = matched
  )
}

#' @title Slice weights data.table for Stan input
#'
#' @param wts_dt Canonicalized `[data.table()]` of weights mapping, corresponding
#'   to the canonicalized `populations` input from `[canonicalize_populations()]`.
#' @param obs_dt Canonicalized `[data.table()]` of observations slice from
#'   `[canonicalize_observations()]`.
#' @param suffix Character suffix appended to output list element names.
#'
#' @return A named list formatted for Stan data consumption.
#'
#' @keywords internal
#' @noRd
#' @autoglobal
slice_weights <- function(wts_dt, obs_dt, suffix) {
  res <- if (nrow(obs_dt) == 0L) {
    list(
      n_obs_unmixed = 0L,
      y_obs_unmixed = integer(0),
      y_smp_unmixed = integer(0),
      w_cohort_unmixed = integer(0),
      w_age_unmixed = integer(0),
      w_dose_unmixed = integer(0),
      w_loc_unmixed = integer(0),
      n_obs_mixed = 0L,
      y_obs_mixed = integer(0),
      y_smp_mixed = integer(0),
      n_weights_mixed = 0L,
      obs_bounds_mixed = integer(0),
      w_cohort_mixed = integer(0),
      w_age_mixed = integer(0),
      w_dose_mixed = integer(0),
      w_loc_mixed = integer(0),
      weights_mixed = numeric(0)
    )
  } else {
    w <- wts_dt[obs_dt, on = .(obs_c_id), nomatch = NULL]
    w_counts <- w[, .(n_w = .N), by = obs_c_id]
    unmixed_c_ids <- w_counts[n_w == 1L, obs_c_id]
    mixed_c_ids <- w_counts[n_w > 1L, obs_c_id]

    obs_unmixed_idx <- which(obs_dt$obs_c_id %in% unmixed_c_ids)
    obs_mixed_idx <- which(obs_dt$obs_c_id %in% mixed_c_ids)

    w_unmixed <- w[obs_c_id %in% unmixed_c_ids]
    if (nrow(w_unmixed) > 0L) {
      unmixed_order <- match(
        obs_dt$obs_c_id[obs_unmixed_idx],
        w_unmixed$obs_c_id
      )
      w_unmixed <- w_unmixed[unmixed_order]
    }

    w_mixed <- w[obs_c_id %in% mixed_c_ids]
    if (nrow(w_mixed) > 0L) {
      mixed_order <- match(w_mixed$obs_c_id, obs_dt$obs_c_id[obs_mixed_idx])
      w_mixed <- w_mixed[order(mixed_order)]
      w_mixed[, range_start := seq_len(.N)]
      w_mixed[, range_start := min(range_start), by = obs_c_id]
      obs_bounds_mixed <- unique(w_mixed$range_start)
    } else {
      obs_bounds_mixed <- integer(0)
    }

    list(
      n_obs_unmixed = length(obs_unmixed_idx),
      y_obs_unmixed = obs_dt$positive[obs_unmixed_idx],
      y_smp_unmixed = obs_dt$sample_n[obs_unmixed_idx],
      w_cohort_unmixed = w_unmixed$cohort,
      w_age_unmixed = w_unmixed$age,
      w_dose_unmixed = w_unmixed$dose,
      w_loc_unmixed = w_unmixed$loc_c_id,
      n_obs_mixed = length(obs_mixed_idx),
      y_obs_mixed = obs_dt$positive[obs_mixed_idx],
      y_smp_mixed = obs_dt$sample_n[obs_mixed_idx],
      n_weights_mixed = nrow(w_mixed),
      obs_bounds_mixed = obs_bounds_mixed,
      w_cohort_mixed = w_mixed$cohort,
      w_age_mixed = w_mixed$age,
      w_dose_mixed = w_mixed$dose,
      w_loc_mixed = w_mixed$loc_c_id,
      weights_mixed = w_mixed$weight
    )
  }
  stats::setNames(res, paste0(names(res), "_", suffix))
}

#' @title Generate initial values for Stan sampling
#'
#' @description
#' Generates a named list of initial parameter values for Stan chains based on
#' empirical survey observations, dose schedule, and location hierarchy.
#'
#' @param dat_stan Named list of data formatted for Stan input.
#' @param model Character string specifying the model formulation (default: `"default"`).
#'
#' @return A named list of initial parameter arrays for Stan.
#'
#' @keywords internal
#' @noRd
generate_inits <- function(dat_stan, model = "default") {
  y_all <- c(
    dat_stan$y_obs_unmixed_uncensored,
    dat_stan$y_obs_mixed_uncensored,
    dat_stan$y_obs_unmixed_right,
    dat_stan$y_obs_mixed_right
  )
  smp_all <- c(
    dat_stan$y_smp_unmixed_uncensored,
    dat_stan$y_smp_mixed_uncensored,
    dat_stan$y_smp_unmixed_right,
    dat_stan$y_smp_mixed_right
  )

  mean_cov <- if (length(y_all) > 0L && sum(smp_all) > 0) {
    sum(y_all) / sum(smp_all)
  } else {
    0.85
  }
  if (is.na(mean_cov) || is.nan(mean_cov)) {
    mean_cov <- 0.85
  }
  baseline_phi <- pmax(0.01, pmin(0.5, 1 - mean_cov))

  inits <- list(
    lambda_raw = array(
      log(rep(3, dat_stan$n_doses)) + stats::rnorm(dat_stan$n_doses, 0, 0.05),
      dim = dat_stan$n_doses
    )
  )

  # Multilayer spatial hierarchy parameters (if present in dat_stan)
  if (isTRUE(dat_stan$n_layers >= 2L)) {
    n_unconstrained <- (dat_stan$n_locs - 1L) - dat_stan$n_parent_locs
    inits$sigma_layer <- array(
      abs(stats::rnorm(dat_stan$n_layers - 1L, 0.5, 0.1)),
      dim = dat_stan$n_layers - 1L
    )
    inits$z_layer <- array(
      stats::rnorm(n_unconstrained, 0, 0.05),
      dim = n_unconstrained
    )
  }

  model_inits <- if (identical(model, "default")) {
    init_beta <- rep(stats::qlogis(baseline_phi), dat_stan$k_bs) +
      stats::rnorm(dat_stan$k_bs, 0, 0.05)
    list(beta_bs = array(init_beta, dim = dat_stan$k_bs))
  } else {
    stop_fmt_if(TRUE, ERR_OPT_UNKNOWN_MODEL, model)
  }

  c(inits, model_inits)
}

#' @title Make Stan initialization function
#'
#' @description
#' Returns a 0-argument function suitable for passing to `fit_model(init = ...)`.
#'
#' @param dat_stan Named list of data formatted for Stan input.
#' @param model Character string specifying the model formulation (default: `"default"`).
#'
#' @return A 0-argument function that returns a list of initial parameter values.
#'
#' @keywords internal
#' @noRd
make_init_fn <- function(dat_stan, model = "default") {
  function() {
    generate_inits(dat_stan, model = model)
  }
}

#' @title Immunity: Geographic & Age-based Projection, `imuGAP`
#'
#' @description
#' Fits the imuGAP Bayesian hierarchical vaccine coverage estimation model across
#' arbitrary user-specified location partitions, birth cohorts, ages, and vaccine
#' doses.
#'
#' @inheritParams canonicalize_observations
#' @inheritParams canonicalize_populations
#' @inheritParams canonicalize_locations
#' @param imugap_opts options for the `imuGAP` model, created by `[imugap_options()]`.
#' @param stan_opts sampler configuration created by `[stan_options()]`
#'   (see `[flexstanr::stan_options()]` for details on supported sampler arguments,
#'   including `iter`, `chains`, `cores`, `seed`, and `backend`).
#'
#' @return An object of class `imugap_fit` wrapping the raw `stanfit` (or
#'   `CmdStanMCMC`) object along with model settings and dataset metadata.
#'
#' @details
#' `sampling()` automatically inspects the depth of the location hierarchy
#' supplied in `locations` via `[canonicalize_locations()]` and `[assemble_layer_data()]`:
#' - **Single-layer (1 layer)**: When only a root location is supplied,
#'   `sampling()` automatically dispatches to the optimized single-location model.
#' - **Multi-layer (>= 2 layers)**: When hierarchical sub-locations are
#'   supplied (e.g., 2-layer state -> county, 3-layer state -> county -> school,
#'   or deeper trees), `sampling()` dispatches to the general hierarchical model
#'   with partial pooling across layer-specific variance components.
#'
#' If the Stan sampler fails to initialize and produces no draws (for the rstan
#' backend, a mode-2 `stanfit` with an empty `@sim`), `sampling()` raises an
#' error of class `imugap_no_draws` rather than returning an empty fit, so the
#' failure can be handled with `tryCatch()`. The check is backend-agnostic (see
#' `backend_has_draws()`).
#'
#' @examplesIf interactive()
#' \donttest{
#' data("locations_sim")
#' data("observations_sim")
#' data("populations_sim")
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
  # check imugap_opts
  model <- imugap_opts$model %||% "default"
  stop_fmt_if(!identical(model, "default"), ERR_OPT_UNKNOWN_MODEL, model)
  dose_sched_opts <- imugap_opts$dose_schedule %||% c(1L, 4L)
  df_opts <- imugap_opts$df %||% 5L

  # check location argument
  loc_info <- canonicalize_locations(locations)
  n_layers <- max(loc_info$layer)
  is_multilayer <- n_layers > 1L
  layer_data <- if (is_multilayer) {
    assemble_layer_data(loc_info)
  } else {
    NULL
  }

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
    df = df_opts,
    intercept = TRUE
  )

  sched_info <- build_interval_schedule(
    dose_sched_opts,
    obs$age,
    max(wts$age)
  )

  st_uncensored <- slice_weights(wts, obs[is.na(censored)], "uncensored")
  st_right <- slice_weights(wts, obs[censored == 1], "right")
  st_left <- slice_weights(wts, obs[0], "left")

  # prepare dat_stan
  dat_stan <- c(
    list(
      n_yr = max(wts$age),
      n_cohort = max(wts$cohort)
    ),
    if (is_multilayer) layer_data,
    list(
      n_doses = length(dose_sched_opts),
      n_intervals = sched_info$n_intervals,
      dt_vec = sched_info$dt_vec,
      dose_sched = sched_info$dose_sched,
      age_to_interval_map = sched_info$age_to_interval_map,
      k_bs = ncol(bsp),
      bs = bsp
    ),
    st_uncensored,
    st_right,
    st_left,
    list(
      predict_mode = 0L,
      num_threads = as.integer(stan_opts$threads_per_chain %||% 1L)
    )
  )

  # The `backend` element is the marker that stan_opts came from stan_options();
  # its absence means a hand-built list. Whatever backend it names wins.
  backend <- stan_opts$backend
  stop_fmt_if(is.null(backend), ERR_STAN_OPTS_CLASS)

  # Select specialized Stan model based on model and hierarchy depth:
  # 1-layer uses the streamlined single-location model; >= 2 layers uses the full
  # hierarchical model.
  model_name <- if (identical(model, "default")) {
    if (is_multilayer) {
      "impute_school_coverage_process_v6"
    } else {
      "impute_school_coverage_process_v6_single_layer"
    }
  } else {
    stop_fmt_if(TRUE, ERR_OPT_UNKNOWN_MODEL, model)
  }

  raw_fit <- fit_model(
    model_name,
    dat_stan,
    init = make_init_fn(dat_stan, model = model),
    stan_opts,
    drop_pars = NULL,
    package = "imuGAP"
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
  stop_fmt_if(!inherits(fit, "imugap_fit"), ERR_NOT_IMUGAP_FIT)
  # Extraction goes through the backend accessor, which only implements the
  # rstan path today; cmdstanr fits expose draws differently, so fail clearly
  # here rather than deep inside the accessor.
  stop_fmt_if(!inherits(fit$stanfit, "stanfit"), ERR_EXTRACT_RSTAN_ONLY)
  backend_extract(fit$stanfit, pars = pars, ...)
}
