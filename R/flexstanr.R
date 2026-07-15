# imuGAP's portable Stan backend layer comes from the flexstanr package (#122).
# It previously lived in a vendored standalone file
# (usethis::use_standalone("ACCIDDA/flexstanr", "backends")); imuGAP now Imports
# flexstanr instead. flexstanr provides the backend-agnostic fit / draws /
# generated-quantities entry points and the sampler-option constructor, and
# resolves the calling package's compiled stanmodels automatically, so imuGAP's
# fits route through it unchanged.
#
# Matching the prior vendored setup, only stan_options() is part of imuGAP's
# public API (re-exported below). fit_model() and the backend_* accessors are
# imported for internal use only.

#' @importFrom flexstanr fit_model backend_draws_array backend_extract backend_generate_quantities backend_has_draws
NULL

#' Stan sampler options
#'
#' imuGAP re-exports \code{\link[flexstanr]{stan_options}} so that
#' \code{imuGAP::stan_options()} continues to work. See the flexstanr
#' documentation for the full argument list and backend semantics.
#'
#' @importFrom flexstanr stan_options
#' @export
flexstanr::stan_options
