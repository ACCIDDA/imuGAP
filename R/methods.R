#' @title Predict coverage probabilities
#'
#' @description
#' Uses the output of `[sampling()]` and a target grid to generate
#' predicted coverage probabilities.
#'
#' @param object an `imugap_fit` object returned by `sampling()`
#' @param target a `[data.frame()]` of target populations to predict for
#' @param ... additional arguments (currently ignored)
#'
#' @details
#' The `[predict()]` method takes an `imugap_fit` object (typically the output of
#' `[sampling()]`) and a target grid (typically output from `[create_target()]`),
#' and generates predicted coverage probabilities for each entry in the target.
#'
#' The `[predict()]` method can be used to generate estimated coverage for any
#' location, cohort, or age considered within the bounds of the original
#' sampling fit. Particularly, this includes enclosing locations without specific
#' observation data, as long as those locations are *somewhere* in the
#' locations hierarchy.
#'
#' @return An object of class `imugap_predict` wrapping the 3D array of predicted
#'   draws and the canonical target dataset.
#'
#' @export
#' @importFrom data.table as.data.table copy data.table
#' @importFrom rstan gqs extract
predict.imugap_fit <- function(
  object,
  target,
  ...
) {
  fit <- object
  if (!inherits(fit, "imugap_fit")) {
    stop("fit must be an object of class 'imugap_fit'", call. = FALSE)
  }

  raw_fit <- fit$stanfit
  target <- create_target(fit, target)
  if (!"obs_id" %in% names(target)) {
    target[, obs_id := obs_c_id]
  }

  # Generate dummy observations
  obs <- canonicalize_observations(
    target[, .(
      obs_id = obs_c_id,
      positive = 0L,
      sample_n = 1L,
      censored = NA_real_
    )]
  )

  # Update the data object for prediction mode
  dat_stan <- fit$data
  dat_stan$n_uncensored_obs <- obs[is.na(censored), .N]
  dat_stan$n_obs <- nrow(obs)
  dat_stan$y_obs <- obs$positive
  dat_stan$y_smp <- obs$sample_n
  dat_stan$n_weights <- nrow(target)
  dat_stan$obs_to_weights_bounds <- seq_len(nrow(target))
  dat_stan$weights_school <- target$loc_c_id
  dat_stan$weights_cohort <- target$cohort
  dat_stan$weights_life_year <- target$age
  dat_stan$weights_dose <- target$dose
  dat_stan$weights <- target$weight
  dat_stan$predict_mode <- 1

  # Run gqs to generate predictions
  gqs_res <- rstan::gqs(
    raw_fit@stanmodel,
    data = dat_stan,
    draws = as.matrix(raw_fit)
  )

  # Extract predictions as 3D array (iterations x chains x parameters)
  p_obs_draws <- as.array(gqs_res, pars = "p_obs")

  structure(
    list(
      draws = p_obs_draws,
      target = target
    ),
    class = "imugap_predict"
  )
}

#' @title Summarize coverage predictions
#'
#' @description
#' Summarizes predicted coverage probabilities from an `imugap_predict` object
#' by location, cohort, age, and dose for the requested quantiles.
#'
#' @param object an `imugap_predict` object returned by `[predict()]`
#' @param probs numeric vector of probabilities/quantiles to compute.
#'   Defaults to `c(0.025, 0.5, 0.975)`.
#' @param ... additional arguments (currently ignored)
#'
#' @return A `data.table` containing target population parameters, posterior mean
#'   coverage (`mean`), and the requested quantiles (e.g. `q2.5`, `q50`, `q97.5`).
#'
#' @export
#' @importFrom stats quantile
summary.imugap_predict <- function(object, probs = c(0.025, 0.5, 0.975), ...) {
  if (!inherits(object, "imugap_predict")) {
    stop("object must be of class 'imugap_predict'", call. = FALSE)
  }

  draws <- object$draws
  target <- data.table::copy(object$target)

  # Compute mean for each target observation over iteration and chain dimensions
  mean_vals <- colMeans(draws, dims = 2)

  # Compute quantiles over iteration and chain dimensions for each variable slice
  quantiles <- t(apply(draws, 3, stats::quantile, probs = probs, na.rm = TRUE))

  # Format column names for the quantiles
  quantile_names <- sprintf("q%g", probs * 100)
  quantile_names <- gsub("\\.", "_", quantile_names)
  colnames(quantiles) <- quantile_names

  stats_dt <- data.table::data.table(
    mean = mean_vals,
    quantiles
  )

  res_dt <- cbind(target, stats_dt)
  res_dt[]
}

#' @title Subset coverage predictions
#'
#' @description
#' Subsets predicted coverage draws by target metadata (variables), iterations,
#' and chains.
#'
#' @param x an `imugap_predict` object returned by `[predict()]`.
#' @param subset logical expression indicating which target variables to keep.
#'   Evaluated in the context of the `target` metadata data.table.
#' @param iteration numeric/integer/logical vector of iterations to keep.
#' @param chain numeric/integer/logical vector of chains to keep.
#' @param ... additional arguments (currently ignored).
#'
#' @return A subsetted `imugap_predict` object with corresponding subsetted `draws`
#'   and `target` metadata.
#'
#' @export
subset.imugap_predict <- function(x, subset, iteration, chain, ...) {
  if (!inherits(x, "imugap_predict")) {
    stop("x must be of class 'imugap_predict'", call. = FALSE)
  }

  # Subset variables (columns/third dimension) using the metadata
  r <- if (missing(subset)) {
    rep_len(TRUE, nrow(x$target))
  } else {
    e <- substitute(subset)
    r <- eval(e, x$target, parent.frame())
    if (!is.logical(r)) {
      stop("'subset' must be logical", call. = FALSE)
    }
    r & !is.na(r)
  }

  # Subset iterations and chains
  iter_idx <- if (missing(iteration)) TRUE else iteration
  chain_idx <- if (missing(chain)) TRUE else chain

  new_draws <- x$draws[iter_idx, chain_idx, r, drop = FALSE]
  new_target <- x$target[r, ]

  structure(
    list(
      draws = new_draws,
      target = new_target
    ),
    class = "imugap_predict"
  )
}
