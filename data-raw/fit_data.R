# Part B of the package-data pipeline: build the fit-derived artifacts.
#
# Produces fit_sim, target_sim, and predict_sim from the tracked *_sim inputs.
# This step needs a Stan toolchain (it compiles and fits the model) but NOT the
# private nc_measles dataset, so it runs in CI as well as locally.
# (latent_params_sim is fit-free and now built as tracked static data by Part A,
# data-raw/DATASET.R -- see #105.)
#
# These three artifacts are NOT tracked in git (see .gitignore); they are
# regenerated on build. Run locally with `just data` (full pipeline) or
# `just data-fit` (this step alone), or directly:
#
#     Rscript data-raw/fit_data.R
#
# fit_sim uses iter = 1000, chains = 4, seed = 1; on-disk it is ~1.2 MB and the
# predict step is sub-sampled to 100 draws to keep predict_sim small (#86).
# stanfit objects reference the compiled Stan model and can be fragile across
# major rstan / StanHeaders updates -- regenerating them here avoids shipping a
# stale binary coupled to an old toolchain.

pkgload::load_all(quiet = TRUE)

# --- Fit -------------------------------------------------------------------
# Run the chains in parallel where it is safe to: the draws are seed-determined,
# so parallel and sequential give identical results, and this fit runs on every
# build (each CI matrix leg). On unix, rstan parallelises via fork, so the forked
# workers inherit the pkgload::load_all()'d compiled model. On Windows rstan uses
# PSOCK worker processes that do NOT inherit it (they fail with "object
# 'rstantools_model_*' not found"), so fall back to sequential there.
fit_cores <- if (.Platform$OS.type == "windows") 1L else 4L
fit_sim <- suppressWarnings(sampling(
  observations_sim,
  populations_sim,
  locations_sim,
  imugap_opts = imugap_options(model_name = "impute_school_coverage_process_odds_rollup"),
  stan_opts = stan_options(
    iter = 1000,
    chains = 4,
    cores = fit_cores,
    refresh = 0,
    seed = 1L
  )
))

# Fail loudly on a degenerate fit rather than silently saving a broken fixture.
# suppressWarnings() above hides rstan's routine sampler warnings, but it would
# also hide a fit where chains failed to initialise, so assert the basics here.
stopifnot(
  inherits(fit_sim$stanfit, "stanfit"),
  all(c("beta_bs", "lambda_raw") %in% fit_sim$stanfit@model_pars),
  all(is.finite(rstan::extract(fit_sim$stanfit, pars = "beta_bs")$beta_bs))
)
save(fit_sim, file = "data/fit_sim.rda", compress = "xz")

# --- Target population for prediction --------------------------------------
target_sim <- canonicalize_target(
  readRDS("data-raw/target_sim.rds"),
  fit_sim
)
save(target_sim, file = "data/target_sim.rda")

# latent_params_sim$coverage is the true coverage for each target_sim row,
# built in Part A over the same create_target() grid spec (#105). The two must
# stay row-aligned; fail loudly here if the grid specs have drifted apart.
stopifnot(length(latent_params_sim$coverage) == nrow(target_sim))

# --- Prediction ------------------------------------------------------------
# Keep a small posterior sub-sample (100 draws) so the bundled fixture stays
# well under CRAN's tarball size limit (#86).
predict_sim <- suppressWarnings(
  predict(object = fit_sim, target = target_sim, posterior_size = 100)
)
save(predict_sim, file = "data/predict_sim.rda", compress = "xz")
