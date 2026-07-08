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
    model_name,
    dat_stan,
    init = NULL,
    stan_opts,
    drop_pars = NULL
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
