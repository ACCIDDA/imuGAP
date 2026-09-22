#' @title Pointwise log-likelihood matrix
#'
#' @description
#' Computes the pointwise log-likelihood matrix for an `imugap_fit` object across
#' all observations and posterior draws.
#'
#' @param object an `imugap_fit` object returned by `[sampling()]`.
#' @param posterior_size optional single positive integer. When set, compute
#'   log-likelihood over only this many draws, taken from the end of each chain.
#'   Defaults to `NULL`, which uses every draw.
#' @param ... additional arguments (currently ignored).
#'
#' @return A numeric matrix of dimensions `S x N`, where `S` is the number of
#'   posterior draws and `N` is the total number of observations in the fit.
#'
#' @examples
#' data("fit_sim", package = "imuGAP")
#' ll <- log_lik(fit_sim, posterior_size = 50)
#' dim(ll)
#'
#' @autoglobal
#' @export
log_lik <- function(object, ...) {
  UseMethod("log_lik")
}

#' @rdname log_lik
#' @method log_lik imugap_fit
#' @export
#' @importFrom stats dbinom pbinom
log_lik.imugap_fit <- function(object, posterior_size = NULL, ...) {
  stop_fmt_if(!inherits(object, "imugap_fit"), ERR_NOT_IMUGAP_FIT, name = "object")

  dat <- object$data
  loc_dt <- object$locations
  # Ensure loc_ids maps by loc_c_id
  loc_ids <- character(max(loc_dt$loc_c_id))
  loc_ids[loc_dt$loc_c_id] <- loc_dt$loc_id

  # Assemble all required target cells across unmixed and mixed observations
  targets_list <- list()
  slice_meta <- list()

  if (isTRUE(dat$n_obs_unmixed_uncensored > 0L)) {
    slice_meta$unmixed_uncensored <- list(
      n_obs = dat$n_obs_unmixed_uncensored,
      y_obs = dat$y_obs_unmixed_uncensored,
      y_smp = dat$y_smp_unmixed_uncensored,
      type = "unmixed_uncensored"
    )
    targets_list$unmixed_uncensored <- data.table::data.table(
      loc_id = loc_ids[dat$w_loc_unmixed_uncensored],
      cohort = dat$w_cohort_unmixed_uncensored,
      age = dat$w_age_unmixed_uncensored,
      dose = dat$w_dose_unmixed_uncensored
    )
  }

  if (isTRUE(dat$n_obs_mixed_uncensored > 0L)) {
    slice_meta$mixed_uncensored <- list(
      n_obs = dat$n_obs_mixed_uncensored,
      y_obs = dat$y_obs_mixed_uncensored,
      y_smp = dat$y_smp_mixed_uncensored,
      n_weights = dat$n_weights_mixed_uncensored,
      obs_bounds = dat$obs_bounds_mixed_uncensored,
      weights = dat$weights_mixed_uncensored,
      type = "mixed_uncensored"
    )
    targets_list$mixed_uncensored <- data.table::data.table(
      loc_id = loc_ids[dat$w_loc_mixed_uncensored],
      cohort = dat$w_cohort_mixed_uncensored,
      age = dat$w_age_mixed_uncensored,
      dose = dat$w_dose_mixed_uncensored
    )
  }

  if (isTRUE(dat$n_obs_unmixed_right > 0L)) {
    slice_meta$unmixed_right <- list(
      n_obs = dat$n_obs_unmixed_right,
      y_obs = dat$y_obs_unmixed_right,
      y_smp = dat$y_smp_unmixed_right,
      type = "unmixed_right"
    )
    targets_list$unmixed_right <- data.table::data.table(
      loc_id = loc_ids[dat$w_loc_unmixed_right],
      cohort = dat$w_cohort_unmixed_right,
      age = dat$w_age_unmixed_right,
      dose = dat$w_dose_unmixed_right
    )
  }

  if (isTRUE(dat$n_obs_mixed_right > 0L)) {
    slice_meta$mixed_right <- list(
      n_obs = dat$n_obs_mixed_right,
      y_obs = dat$y_obs_mixed_right,
      y_smp = dat$y_smp_mixed_right,
      n_weights = dat$n_weights_mixed_right,
      obs_bounds = dat$obs_bounds_mixed_right,
      weights = dat$weights_mixed_right,
      type = "mixed_right"
    )
    targets_list$mixed_right <- data.table::data.table(
      loc_id = loc_ids[dat$w_loc_mixed_right],
      cohort = dat$w_cohort_mixed_right,
      age = dat$w_age_mixed_right,
      dose = dat$w_dose_mixed_right
    )
  }

  if (isTRUE(dat$n_obs_unmixed_left > 0L)) {
    slice_meta$unmixed_left <- list(
      n_obs = dat$n_obs_unmixed_left,
      y_obs = dat$y_obs_unmixed_left,
      y_smp = dat$y_smp_unmixed_left,
      type = "unmixed_left"
    )
    targets_list$unmixed_left <- data.table::data.table(
      loc_id = loc_ids[dat$w_loc_unmixed_left],
      cohort = dat$w_cohort_unmixed_left,
      age = dat$w_age_unmixed_left,
      dose = dat$w_dose_unmixed_left
    )
  }

  if (isTRUE(dat$n_obs_mixed_left > 0L)) {
    slice_meta$mixed_left <- list(
      n_obs = dat$n_obs_mixed_left,
      y_obs = dat$y_obs_mixed_left,
      y_smp = dat$y_smp_mixed_left,
      n_weights = dat$n_weights_mixed_left,
      obs_bounds = dat$obs_bounds_mixed_left,
      weights = dat$weights_mixed_left,
      type = "mixed_left"
    )
    targets_list$mixed_left <- data.table::data.table(
      loc_id = loc_ids[dat$w_loc_mixed_left],
      cohort = dat$w_cohort_mixed_left,
      age = dat$w_age_mixed_left,
      dose = dat$w_dose_mixed_left
    )
  }

  if (length(targets_list) == 0L) {
    return(matrix(numeric(0), nrow = 0L, ncol = 0L))
  }

  all_targets <- data.table::rbindlist(targets_list)
  pred <- predict(object, target = all_targets, posterior_size = posterior_size)

  # Reshape to 2D draws matrix: S x N_cells
  draws_mat <- apply(pred$draws, 3L, c)
  n_draws <- nrow(draws_mat)

  ll_pieces <- list()
  col_offset <- 0L

  for (s_name in names(slice_meta)) {
    meta <- slice_meta[[s_name]]
    if (identical(meta$type, "unmixed_uncensored")) {
      cols <- seq.int(col_offset + 1L, col_offset + meta$n_obs)
      p_mat <- draws_mat[, cols, drop = FALSE]
      y_rep <- rep(meta$y_obs, each = n_draws)
      smp_rep <- rep(meta$y_smp, each = n_draws)
      ll_mat <- matrix(
        stats::dbinom(y_rep, smp_rep, as.vector(p_mat), log = TRUE),
        nrow = n_draws,
        ncol = meta$n_obs
      )
      ll_pieces[[s_name]] <- ll_mat
      col_offset <- col_offset + meta$n_obs
    } else if (identical(meta$type, "mixed_uncensored")) {
      n_obs <- meta$n_obs
      b_start <- meta$obs_bounds
      b_end <- c(tail(meta$obs_bounds, -1L) - 1L, meta$n_weights)
      ll_mat <- matrix(0.0, nrow = n_draws, ncol = n_obs)
      wts <- meta$weights
      for (i in seq_len(n_obs)) {
        w_idx <- seq.int(b_start[i], b_end[i])
        cols <- col_offset + w_idx
        p_cells <- draws_mat[, cols, drop = FALSE]
        p_agg <- as.vector(p_cells %*% wts[w_idx])
        ll_mat[, i] <- stats::dbinom(
          meta$y_obs[i],
          meta$y_smp[i],
          p_agg,
          log = TRUE
        )
      }
      ll_pieces[[s_name]] <- ll_mat
      col_offset <- col_offset + meta$n_weights
    } else if (identical(meta$type, "unmixed_right")) {
      cols <- seq.int(col_offset + 1L, col_offset + meta$n_obs)
      p_mat <- draws_mat[, cols, drop = FALSE]
      y_fail <- meta$y_smp - meta$y_obs
      y_fail_rep <- rep(y_fail, each = n_draws)
      smp_rep <- rep(meta$y_smp, each = n_draws)
      ll_mat <- matrix(
        stats::pbinom(
          y_fail_rep,
          smp_rep,
          1.0 - as.vector(p_mat),
          log.p = TRUE
        ),
        nrow = n_draws,
        ncol = meta$n_obs
      )
      ll_pieces[[s_name]] <- ll_mat
      col_offset <- col_offset + meta$n_obs
    } else if (identical(meta$type, "mixed_right")) {
      n_obs <- meta$n_obs
      b_start <- meta$obs_bounds
      b_end <- c(tail(meta$obs_bounds, -1L) - 1L, meta$n_weights)
      ll_mat <- matrix(0.0, nrow = n_draws, ncol = n_obs)
      wts <- meta$weights
      y_fail <- meta$y_smp - meta$y_obs
      for (i in seq_len(n_obs)) {
        w_idx <- seq.int(b_start[i], b_end[i])
        cols <- col_offset + w_idx
        p_cells <- draws_mat[, cols, drop = FALSE]
        p_agg <- as.vector(p_cells %*% wts[w_idx])
        ll_mat[, i] <- stats::pbinom(
          y_fail[i],
          meta$y_smp[i],
          1.0 - p_agg,
          log.p = TRUE
        )
      }
      ll_pieces[[s_name]] <- ll_mat
      col_offset <- col_offset + meta$n_weights
    } else if (identical(meta$type, "unmixed_left")) {
      cols <- seq.int(col_offset + 1L, col_offset + meta$n_obs)
      p_mat <- draws_mat[, cols, drop = FALSE]
      y_rep <- rep(meta$y_obs, each = n_draws)
      smp_rep <- rep(meta$y_smp, each = n_draws)
      ll_mat <- matrix(
        stats::pbinom(y_rep, smp_rep, as.vector(p_mat), log.p = TRUE),
        nrow = n_draws,
        ncol = meta$n_obs
      )
      ll_pieces[[s_name]] <- ll_mat
      col_offset <- col_offset + meta$n_obs
    } else if (identical(meta$type, "mixed_left")) {
      n_obs <- meta$n_obs
      b_start <- meta$obs_bounds
      b_end <- c(tail(meta$obs_bounds, -1L) - 1L, meta$n_weights)
      ll_mat <- matrix(0.0, nrow = n_draws, ncol = n_obs)
      wts <- meta$weights
      for (i in seq_len(n_obs)) {
        w_idx <- seq.int(b_start[i], b_end[i])
        cols <- col_offset + w_idx
        p_cells <- draws_mat[, cols, drop = FALSE]
        p_agg <- as.vector(p_cells %*% wts[w_idx])
        ll_mat[, i] <- stats::pbinom(
          meta$y_obs[i],
          meta$y_smp[i],
          p_agg,
          log.p = TRUE
        )
      }
      ll_pieces[[s_name]] <- ll_mat
      col_offset <- col_offset + meta$n_weights
    }
  }

  res <- do.call(cbind, ll_pieces)
  colnames(res) <- paste0("obs[", seq_len(ncol(res)), "]")
  res
}

#' @title Leave-one-out cross-validation for imuGAP models
#'
#' @description
#' Computes approximate leave-one-out cross-validation (LOO-CV) using Pareto
#' smoothed importance sampling (PSIS-LOO) via the `{loo}` package.
#'
#' @param x an `imugap_fit` object returned by `[sampling()]`.
#' @param posterior_size optional single positive integer specifying how many
#'   draws to use from the end of each chain. Defaults to `NULL`.
#' @param ... additional arguments passed to `[loo::loo()]` (e.g. `cores`, `r_eff`).
#'
#' @return A `loo` object as returned by `loo::loo()`.
#'
#' @examplesIf interactive()
#' \donttest{
#' data("fit_sim", package = "imuGAP")
#' loo_res <- loo(fit_sim, posterior_size = 100)
#' print(loo_res)
#' }
#'
#' @export
loo <- function(x, ...) {
  UseMethod("loo")
}

#' @rdname loo
#' @method loo imugap_fit
#' @export
#' @exportS3Method loo::loo
loo.imugap_fit <- function(x, posterior_size = NULL, ...) {
  stop_fmt_if(!inherits(x, "imugap_fit"), ERR_NOT_IMUGAP_FIT, name = "x")
  stop_fmt_if(!requireNamespace("loo", quietly = TRUE), ERR_LOO_NOT_INSTALLED)

  ll <- log_lik(x, posterior_size = posterior_size)
  loo::loo(ll, ...)
}
