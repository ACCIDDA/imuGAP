# Part B of the package-data pipeline: build the fit-derived artifacts.
#
# Produces fit_sim, target_sim, and predict_sim from the tracked *_sim inputs,
# as well as 1-layer and 2-layer variants (fit_sim_1layer, predict_sim_1layer,
# fit_sim_2layer, predict_sim_2layer) for layer demonstration and vignettes.

pkgload::load_all(quiet = TRUE)
library(data.table)

fit_cores <- if (.Platform$OS.type == "windows") 1L else 4L
st_opts <- stan_options(
  iter = 1000,
  chains = 4,
  cores = fit_cores,
  refresh = 0,
  seed = 1L
)

# --- 3-Layer Fit (State -> County -> School) -------------------------------
fit_sim <- suppressWarnings(sampling(
  observations_sim,
  populations_sim,
  locations_sim,
  stan_opts = st_opts
))

stopifnot(
  inherits(fit_sim$stanfit, "stanfit"),
  all(c("beta_bs", "lambda_raw") %in% fit_sim$stanfit@model_pars),
  all(is.finite(rstan::extract(fit_sim$stanfit, pars = "beta_bs")$beta_bs))
)
usethis::use_data(fit_sim, overwrite = TRUE, compress = "xz")

target_sim <- canonicalize_target(
  create_target(
    location = unique(locations_sim$loc_id),
    age = 1:18,
    cohort = max(populations_sim$cohort) - 18,
    dose = c(1, 2),
    mode = "snapshot"
  ),
  fit_sim
)
usethis::use_data(target_sim, overwrite = TRUE, compress = "xz")

stopifnot(length(latent_params_sim$coverage) == nrow(target_sim))

predict_sim <- suppressWarnings(
  predict(object = fit_sim, target = target_sim, posterior_size = 100)
)
usethis::use_data(predict_sim, overwrite = TRUE, compress = "xz")

# --- 2-Layer Fit (State -> County) ----------------------------------------
locations_sim_2layer <- locations_sim[is.na(parent_id) | parent_id == "State"]

populations_sim_2layer <- copy(populations_sim)
loc_map_2layer <- locations_sim[!is.na(parent_id), .(loc_id, parent_id)]
populations_sim_2layer[loc_map_2layer, on = .(loc_id), loc_id := i.parent_id]
populations_sim_2layer <- populations_sim_2layer[,
  .(weight = sum(weight)),
  by = .(obs_id, loc_id, cohort, age, dose)
]
observations_sim_2layer <- copy(observations_sim)

fit_sim_2layer <- suppressWarnings(sampling(
  observations_sim_2layer,
  populations_sim_2layer,
  locations_sim_2layer,
  stan_opts = st_opts
))

stopifnot(inherits(fit_sim_2layer$stanfit, "stanfit"))
usethis::use_data(fit_sim_2layer, overwrite = TRUE, compress = "xz")

target_sim_2layer <- canonicalize_target(
  create_target(
    location = unique(locations_sim_2layer$loc_id),
    age = 1:18,
    cohort = max(populations_sim_2layer$cohort) - 18,
    dose = c(1, 2),
    mode = "snapshot"
  ),
  fit_sim_2layer
)
usethis::use_data(target_sim_2layer, overwrite = TRUE, compress = "xz")

predict_sim_2layer <- suppressWarnings(
  predict(
    object = fit_sim_2layer,
    target = target_sim_2layer,
    posterior_size = 100
  )
)
usethis::use_data(predict_sim_2layer, overwrite = TRUE, compress = "xz")

# --- 1-Layer Fit (State Only) ---------------------------------------------
locations_sim_1layer <- locations_sim[is.na(parent_id)]

populations_sim_1layer <- copy(populations_sim)
populations_sim_1layer[, loc_id := "State"]
populations_sim_1layer <- populations_sim_1layer[,
  .(weight = sum(weight)),
  by = .(obs_id, loc_id, cohort, age, dose)
]
observations_sim_1layer <- copy(observations_sim)

fit_sim_1layer <- suppressWarnings(sampling(
  observations_sim_1layer,
  populations_sim_1layer,
  locations_sim_1layer,
  stan_opts = st_opts
))

stopifnot(inherits(fit_sim_1layer$stanfit, "stanfit"))
usethis::use_data(fit_sim_1layer, overwrite = TRUE, compress = "xz")

target_sim_1layer <- canonicalize_target(
  create_target(
    location = "State",
    age = 1:18,
    cohort = max(populations_sim_1layer$cohort) - 18,
    dose = c(1, 2),
    mode = "snapshot"
  ),
  fit_sim_1layer
)
usethis::use_data(target_sim_1layer, overwrite = TRUE, compress = "xz")

predict_sim_1layer <- suppressWarnings(
  predict(
    object = fit_sim_1layer,
    target = target_sim_1layer,
    posterior_size = 100
  )
)
usethis::use_data(predict_sim_1layer, overwrite = TRUE, compress = "xz")
