# Part B of the package-data pipeline: build the fitted-data artifacts.
#
# Produces fit_sim, target_sim, latent_params_sim, and predict_sim from the
# tracked *_sim inputs plus the simulation internals saved by Part A
# (data-raw/DATASET.R -> data-raw/sim_internals.rds). This step needs a Stan
# toolchain (it compiles and fits the model) but NOT the private nc_measles
# dataset, so it runs in CI as well as locally.
#
# These four artifacts are NOT tracked in git (see .gitignore); they are
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

internals <- readRDS("data-raw/sim_internals.rds")

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
target_sim <- canonicalize_target(create_target(
  location = unique(locations_sim$loc_id),
  age = 1:18,
  cohort = max(populations_sim$cohort) - 18,
  dose = c(1, 2),
  mode = "snapshot"
), fit_sim)
save(target_sim, file = "data/target_sim.rda")

# --- Latent parameters + true coverage for target_sim ----------------------
# Background ("true") coverage for each target_sim row, reconstructed from the
# simulation internals (offsets, coverage curves) saved by Part A.
target_sim_dt <- data.table::as.data.table(target_sim)
p_true <- numeric(nrow(target_sim_dt))
for (i in seq_len(nrow(target_sim_dt))) {
  loc <- target_sim_dt$loc_id[i]
  cohort_val <- target_sim_dt$cohort[i]
  age_val <- target_sim_dt$age[i]
  dose_val <- target_sim_dt$dose[i]

  if (loc == "State") {
    offset <- 0
  } else if (loc %in% internals$county_names) {
    c_idx <- match(loc, internals$county_names)
    offset <- internals$off_cnty[c_idx]
  } else {
    s_idx <- match(loc, internals$school_names)
    offset <- internals$off_sch[s_idx] +
      internals$off_cnty[internals$cnty_ids[s_idx]]
  }

  p_true[i] <- stats::plogis(
    stats::qlogis(internals$phi_st[cohort_val]) + offset
  ) *
    internals$uptake[age_val, dose_val]
}

latent_params_sim <- list(
  phi_state = internals$phi_st,
  lambda = internals$lambda,
  sigma_sch = internals$sigma_sch,
  sigma_cnty = internals$sigma_cnty,
  off_sch = internals$off_sch,
  off_cnty = internals$off_cnty,
  censor_reduction = internals$censor_reduction,
  uptake = internals$uptake,
  coverage = p_true
)
save(latent_params_sim, file = "data/latent_params_sim.rda")

# --- Prediction ------------------------------------------------------------
# Keep a small posterior sub-sample (100 draws) so the bundled fixture stays
# well under CRAN's tarball size limit (#86).
predict_sim <- suppressWarnings(
  predict(object = fit_sim, target = target_sim, posterior_size = 100)
)
save(predict_sim, file = "data/predict_sim.rda", compress = "xz")
