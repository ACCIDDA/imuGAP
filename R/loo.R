#' @title Pointwise log-likelihood matrix for `imugap_fit`
#'
#' @description
#' Computes the pointwise log-likelihood matrix for an `imugap_fit` object across
#' all observations and posterior draws.
#'
#' @param object an `imugap_fit` object returned by [sampling()].
#' @param posterior_size optional integer scalar; how many draws to use from the
#'   end of each chain? (default: `NULL`, which uses all draws).
#' @param ... additional arguments (currently ignored).
#'
#' @return for `log_lik.imugap_fit()`: a numeric matrix of dimensions `S x N`,
#'   where `S` is the number of posterior draws and `N` is the total number of
#'   observations in the fit.
#'
#' @examplesIf interactive()
#' \donttest{
#' data("fit_sim", package = "imuGAP")
#' ll <- rstantools::log_lik(fit_sim, posterior_size = 50)
#' dim(ll)
#' }
#'
#' @importFrom rstantools log_lik
#' @exportS3Method rstantools::log_lik
log_lik.imugap_fit <- function(object, posterior_size = NULL, ...) {
  stop_fmt_if(
    !inherits(object, "imugap_fit"),
    ERR_NOT_IMUGAP_FIT,
    name = "object"
  )

  raw_fit <- object$raw_fit
  draws_array <- backend_draws_array(raw_fit)
  param_names <- dimnames(draws_array)[[3]]
  ll_param_idx <- grep("^log_lik\\[", param_names)

  if (length(ll_param_idx) > 0L) {
    # log_lik was already computed during sampling
    draws_sub <- subset_draws_tail(draws_array, posterior_size)
    ll_sub <- draws_sub[,
      seq_len(dim(draws_sub)[2]),
      ll_param_idx,
      drop = FALSE
    ]
    res <- apply(ll_sub, 3L, c)
  } else {
    # Compute log-likelihood via generated quantities through predict
    pred <- predict(
      object,
      posterior_size = posterior_size,
      compute_log_lik = TRUE
    )
    res <- apply(pred$draws, 3L, c)
  }

  colnames(res) <- paste0("obs[", seq_len(ncol(res)), "]")
  res
}

#' @title Leave-one-out cross-validation for `imugap_fit`
#'
#' @description
#' Computes approximate leave-one-out cross-validation (LOO-CV) using Pareto
#' smoothed importance sampling (PSIS-LOO) via the `{loo}` package.
#'
#' @param x an `imugap_fit` object returned by [sampling()].
#' @param posterior_size optional integer scalar; how many draws to use from the
#'   end of each chain? (default: `NULL`, which uses all draws).
#' @param ... additional arguments passed to [loo::loo()] (e.g. `cores`, `r_eff`).
#'
#' @return for `loo.imugap_fit()`: an object of class `loo`, as returned by [loo::loo()].
#'
#' @examplesIf interactive() && requireNamespace("loo", quietly = TRUE)
#' \donttest{
#' data("fit_sim", package = "imuGAP")
#' loo_res <- loo::loo(fit_sim, posterior_size = 100)
#' print(loo_res)
#' }
#'
#' @exportS3Method loo::loo
loo.imugap_fit <- function(x, posterior_size = NULL, ...) {
  stop_fmt_if(!inherits(x, "imugap_fit"), ERR_NOT_IMUGAP_FIT, name = "x")
  stop_fmt_if(!requireNamespace("loo", quietly = TRUE), ERR_LOO_NOT_INSTALLED)

  ll <- log_lik.imugap_fit(x, posterior_size = posterior_size)
  loo::loo(ll, ...)
}
