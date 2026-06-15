#' @title Predict coverage probabilities
#'
#' @description
#' Uses the output of `[sampling()]` and a target grid to generate
#' predicted coverage probabilities.
#'
#' @param object an `imugap_fit` object returned by `sampling()`
#' @param target a `[data.frame()]` of target populations to predict for
#' @param posterior_size optional single positive integer giving the number of
#'   posterior draws to predict over. Draws are taken from the end of each chain
#'   (the converged tail) and apportioned as evenly as possible across chains,
#'   and the value must not exceed the number of draws in the fit. Defaults to
#'   `NULL`, which uses every available draw.
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
#' `posterior_size` to predict over a smaller number of draws taken from the
#' converged end of each chain; this is how the bundled `predict_sim` fixture is
#' kept small. `predict()` does not yet check whether the retained draws are
#' adequate (chain mixing, effective sample size) and always warns to that
#' effect.
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
#' @importFrom rstan gqs extract
predict.imugap_fit <- function(
  object,
  target,
  posterior_size = NULL,
  ...
) {
  fit <- object
  if (!inherits(fit, "imugap_fit")) {
    stop("fit must be an object of class 'imugap_fit'", call. = FALSE)
  }

  raw_fit <- fit$stanfit

  # Posterior draws available across all chains (post-warmup).
  draws_arr <- as.array(raw_fit)
  n_iter <- dim(draws_arr)[1]
  n_chains <- dim(draws_arr)[2]
  n_avail <- n_iter * n_chains

  # Hard error on a nonsensical requested posterior size.
  if (!is.null(posterior_size)) {
    if (
      !is.numeric(posterior_size) || length(posterior_size) != 1L ||
        is.na(posterior_size) || posterior_size <= 0 ||
        posterior_size != as.integer(posterior_size)
    ) {
      stop("`posterior_size` must be a single positive integer.", call. = FALSE)
    }
    if (posterior_size > n_avail) {
      stop(
        sprintf(
          "`posterior_size` (%d) exceeds the %d posterior draws in the fit.",
          as.integer(posterior_size),
          n_avail
        ),
        call. = FALSE
      )
    }
  }

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

  # No adequacy check yet (chain mixing, effective sample size); warn so callers
  # verify the retained draws are sufficient themselves.
  warning(
    "predict() does not check whether the posterior draws are adequate ",
    "(chain mixing, effective sample size); verify the sufficiency statistics ",
    "yourself.",
    call. = FALSE
  )

  # Select the posterior draws to predict over. With `posterior_size`, keep that
  # many draws from the end of each chain (the converged tail), apportioned as
  # evenly as possible across chains; otherwise use every draw.
  if (is.null(posterior_size)) {
    draws_mat <- as.matrix(raw_fit)
  } else {
    size <- as.integer(posterior_size)
    base <- size %/% n_chains
    extra <- size %% n_chains
    per_chain <- rep(base, n_chains) +
      c(rep(1L, extra), rep(0L, n_chains - extra))
    par_names <- dimnames(draws_arr)[[3]]
    chain_mats <- lapply(seq_len(n_chains), function(ch) {
      k <- per_chain[ch]
      if (k == 0L) {
        return(NULL)
      }
      rows <- seq.int(n_iter - k + 1L, n_iter)
      m <- matrix(draws_arr[rows, ch, ], nrow = k, ncol = length(par_names))
      colnames(m) <- par_names
      m
    })
    draws_mat <- do.call(rbind, chain_mats)
  }

  # Run gqs to generate predictions
  gqs_res <- rstan::gqs(
    raw_fit@stanmodel,
    data = dat_stan,
    draws = draws_mat
  )

  # Extract predictions as 3D array (iterations x chains x parameters)
  p_obs_draws <- as.array(gqs_res, pars = "p_obs")
  if (length(dim(p_obs_draws)) == 2L) {
    dim(p_obs_draws) <- c(dim(p_obs_draws)[1], 1L, dim(p_obs_draws)[2])
  }

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
#' @examples
#' # Load example prediction object
#' data("predict_sim", package = "imuGAP")
#'
#' # Subset predictions by target metadata
#' subset(predict_sim, dose == 2)
#'
#' # Subset predictions by iteration and chain
#' subset(predict_sim, iteration = 1:50, chain = 1)
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
  if (!inherits(x, "imugap_predict")) {
    stop("x must be of class 'imugap_predict'", call. = FALSE)
  }

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
