# Internal error message format strings for methods.R
ERR_NOT_IMUGAP_FIT <- "`fit` must be an object of class 'imugap_fit'"
ERR_NOT_RSTAN_BACKEND <- paste0(
  "predict() currently supports only the 'rstan' backend; refit with ",
  "stan_options(backend = 'rstan')"
)
ERR_POSTERIOR_SIZE_SINGLE <- "`posterior_size` must be a single value"
ERR_POSTERIOR_SIZE_EXCEEDS <- paste0(
  "`posterior_size` (%d) exceeds the %d available posterior ",
  "draws in `fit`"
)
ERR_NOT_IMUGAP_PREDICT <- "`%s` must be an object of class 'imugap_predict'"
ERR_SUBSET_NOT_LOGICAL <- "`subset` must be a logical vector"
MSG_POSTERIOR_SIZE_ROUNDED <- paste0(
  "`posterior_size` (%d) is not a multiple of the %d chains; ",
  "using %d draws instead"
)
MSG_POSTERIOR_SUBSAMPLE_WARN <- paste0(
  "predict() is using a sub-sample of %d posterior draws and does ",
  "not check whether it is adequate (chain mixing, effective sample ",
  "size); verify sufficiency statistics yourself"
)

#' @title Predict coverage probabilities
#'
#' @description
#' Uses the output of `[sampling()]` and a target grid to generate
#' predicted coverage probabilities.
#'
#' @param object an `imugap_fit` object returned by `sampling()`
#' @param target a `[data.frame()]` of target populations to predict for
#' @param posterior_size optional single positive integer. When set, predict
#'   over only this many draws, taken from the end of each chain (the converged
#'   tail). Must be a multiple of the number of chains; a value that isn't is
#'   rounded up to the next multiple, with a warning. Must not exceed the number
#'   of draws in the fit. Defaults to `NULL`, which uses every draw.
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
#' By default `predict()` uses every posterior draw in the fit. Supply
#' `posterior_size` to predict over a sub-sample taken from the end of each
#' chain; this is how the bundled `predict_sim` fixture is kept small. The
#' returned draws keep the per-chain structure (iterations x chains x targets).
#' When a sub-sample is taken `predict()` warns that it has not checked whether
#' those draws are adequate (chain mixing, effective sample size).
#'
#' @return An object of class `imugap_predict` wrapping the 3D array of predicted
#'   draws and the canonical target dataset.
#'
#' @examples
#' \donttest{
#' # Load example fit object and target population
#' data("fit_sim", package = "imuGAP")
#' data("target_sim", package = "imuGAP")
#'
#' # Generate predictions over 100 posterior draws
#' preds <- predict(fit_sim, target = target_sim, posterior_size = 100)
#' }
#'
#' @export
#' @importFrom data.table as.data.table copy data.table
predict.imugap_fit <- function(
  object,
  target,
  posterior_size = NULL,
  ...
) {
  fit <- object
  stop_fmt_if(!inherits(fit, "imugap_fit"), ERR_NOT_IMUGAP_FIT)

  raw_fit <- fit$stanfit
  # predict() runs generated quantities through the backend accessors, which
  # only implement the rstan path today. cmdstanr fits return a CmdStanMCMC;
  # their generated-quantities support is a separate piece of work, so fail
  # clearly here rather than deep inside the accessor.
  stop_fmt_if(!inherits(raw_fit, "stanfit"), ERR_NOT_RSTAN_BACKEND)

  # Posterior draws as a 3D array: iterations x chains x parameters.
  draws_array <- backend_draws_array(raw_fit)
  n_iter <- dim(draws_array)[1]
  n_chains <- dim(draws_array)[2]
  n_avail <- n_iter * n_chains

  if (!is.null(posterior_size)) {
    posterior_size <- assert_positive_int(posterior_size, "posterior_size")
    stop_fmt_if(length(posterior_size) != 1L, ERR_POSTERIOR_SIZE_SINGLE)
    # The slice keeps an equal number of draws from the end of each chain, so
    # the size must be a multiple of the chain count; round up if it isn't.
    rounded <- as.integer(ceiling(posterior_size / n_chains) * n_chains)
    warn_fmt_if(
      posterior_size != rounded,
      MSG_POSTERIOR_SIZE_ROUNDED,
      posterior_size,
      n_chains,
      rounded
    )
    posterior_size <- rounded

    stop_fmt_if(
      posterior_size > n_avail,
      ERR_POSTERIOR_SIZE_EXCEEDS,
      posterior_size,
      n_avail
    )
    # No adequacy check (mixing, ESS); warn only when a sub-sample is taken.
    warn_fmt_if(
      TRUE,
      MSG_POSTERIOR_SUBSAMPLE_WARN,
      posterior_size
    )
  }

  target <- canonicalize_target(target, fit)

  # Update the data object for prediction mode
  dat_stan <- fit$data
  dat_stan$n_uncensored_obs <- nrow(target)
  dat_stan$n_obs <- nrow(target)
  dat_stan$y_obs <- rep(0L, nrow(target))
  dat_stan$y_smp <- rep(1L, nrow(target))
  dat_stan$n_weights <- nrow(target)
  dat_stan$obs_to_weights_bounds <- seq_len(nrow(target))
  dat_stan$weights_school <- target$loc_c_id
  dat_stan$weights_cohort <- target$cohort
  dat_stan$weights_life_year <- target$age
  dat_stan$weights_dose <- target$dose
  dat_stan$weights <- target$weight
  dat_stan$predict_mode <- 1

  # Slice the iterations dimension, keeping an equal number of draws from the
  # end of each chain (the converged tail); otherwise use every draw.
  draws_sub <- if (is.null(posterior_size)) {
    draws_array
  } else {
    keep <- posterior_size %/% n_chains
    draws_array[seq.int(n_iter - keep + 1L, n_iter), , , drop = FALSE]
  }
  n_keep <- dim(draws_sub)[1]

  # Flatten to the 2D draws matrix gqs expects (rows = draws, cols = params).
  draws_mat <- apply(draws_sub, 3L, c)

  # Predicted coverage via the backend's generated-quantities run, reshaped to
  # iterations x chains x targets so the per-chain structure is preserved.
  p_obs_mat <- backend_generate_quantities(
    raw_fit,
    dat_stan,
    draws_mat,
    "p_obs"
  )
  p_obs_draws <- array(p_obs_mat, dim = c(n_keep, n_chains, ncol(p_obs_mat)))

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
#' @examples
#' # Load example prediction object
#' data("predict_sim", package = "imuGAP")
#'
#' # Summarize coverage predictions
#' summary(predict_sim)
#'
#' # Summarize with custom quantiles
#' summary(predict_sim, probs = c(0.1, 0.5, 0.9))
#'
#' @method summary imugap_predict
#' @export
#' @importFrom stats quantile
summary.imugap_predict <- function(object, probs = c(0.025, 0.5, 0.975), ...) {
  stop_fmt_if(
    !inherits(object, "imugap_predict"),
    ERR_NOT_IMUGAP_PREDICT,
    "object"
  )

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
#' @examples
#' # Load example prediction object
#' data("predict_sim", package = "imuGAP")
#'
#' # Subset predictions by target metadata
#' subset(predict_sim, dose == 2)
#'
#' # Subset predictions by iteration and chain
#' subset(predict_sim, iteration = 1:10, chain = 1)
#'
#' @export
subset.imugap_predict <- function(x, subset, iteration, chain, ...) {
  stop_fmt_if(!inherits(x, "imugap_predict"), ERR_NOT_IMUGAP_PREDICT, "x")

  # Subset variables (columns/third dimension) using the metadata
  r <- if (missing(subset)) {
    rep_len(TRUE, nrow(x$target))
  } else {
    e <- substitute(subset)
    r <- eval(e, x$target, parent.frame())
    stop_fmt_if(!is.logical(r), ERR_SUBSET_NOT_LOGICAL)
    r & !is.na(r)
  }

  # Subset iterations and chains
  iter_idx <- if (missing(iteration)) seq_len(dim(x$draws)[1]) else iteration
  chain_idx <- if (missing(chain)) seq_len(dim(x$draws)[2]) else chain

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

#' @title Convert coverage predictions to a data.frame
#'
#' @description
#' Converts the 3D draws array of an `imugap_predict` object into a long-format
#' `data.frame` containing `iteration`, `chain`, target metadata, and a
#' `coverage` column.
#'
#' @param x an `imugap_predict` object returned by `[predict()]`.
#' @param row.names `NULL` or a character vector giving the row names for the
#'   data frame.
#' @param optional logical. If `TRUE`, setting row names and converting column
#'   names is optional.
#' @param ... additional arguments (currently ignored).
#'
#' @return A `data.table` with columns `iteration`, `chain`, the target metadata
#'   columns, and `coverage`.
#'
#' @examples
#' # Load example prediction object
#' data("predict_sim", package = "imuGAP")
#'
#' # Convert predictions to a data.frame/data.table
#' df <- as.data.frame(predict_sim)
#' head(df)
#'
#' @export
as.data.frame.imugap_predict <- function(
  x,
  row.names = NULL, # nolint
  optional = FALSE,
  ...
) {
  stop_fmt_if(!inherits(x, "imugap_predict"), ERR_NOT_IMUGAP_PREDICT, "x")

  dims <- dim(x$draws)
  dim_i <- dims[1]
  dim_c <- dims[2]
  dim_v <- dims[3]

  iter_vals <- seq_len(dim_i)
  chain_vals <- seq_len(dim_c)

  iterations <- rep(iter_vals, times = dim_c * dim_v)
  chains <- rep(rep(chain_vals, each = dim_i), times = dim_v)
  coverage <- as.vector(x$draws)

  target_rep <- data.table::copy(x$target)
  target_rep <- target_rep[rep(seq_len(dim_v), each = dim_i * dim_c), ]

  res <- data.table::data.table(
    iteration = iterations,
    chain = chains,
    target_rep,
    coverage = coverage
  )

  res[]
}
