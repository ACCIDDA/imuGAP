# Part A of the package-data pipeline: simulate the *_sim inputs and the static
# latent-parameter fixtures for both logit and probit links.
#
# This step depends on the private nc_measles dataset (read below) and so cannot
# run in CI; the resulting *_sim inputs and latent_params_sim are tracked in git.
# It also writes data-raw/sim_internals.rds and data-raw/sim_internals_probit.rds,
# consumed by Part B (data-raw/fit_data.R) to build the genuinely fit-derived artifacts
# (fit_sim/target_sim/predict_sim) without re-running this simulation.
# Run with `just data` (or `just data-inputs` for this step alone).

library(data.table)

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_code()
} else {
  stop("pkgload not found")
}

# Source simulation helper functions
source("data-raw/dataset_helpers.R")

# Base simulation setup with fixed random seed and parameters
setup <- get_simulation_setup(
  seed = 93254,
  sigma_sch = 0.8,
  sigma_cnty = 0.4
)

# 1. Logit link simulation with systematic mu offset aggregation
latent_logit <- generate_latent_current(setup, link = "logit")
sim_data_logit <- simulate_observations_from_latent(
  setup,
  latent_logit,
  uncensored = FALSE
)

observations_sim <- sim_data_logit$observations_sim
populations_sim <- sim_data_logit$populations_sim
locations_sim <- sim_data_logit$locations_sim
latent_params_sim <- sim_data_logit$latent_params_sim
sim_internals <- sim_data_logit$sim_internals
target_sim <- sim_data_logit$target_sim

usethis::use_data(observations_sim, overwrite = TRUE, compress = "xz")
usethis::use_data(populations_sim, overwrite = TRUE, compress = "xz")
usethis::use_data(locations_sim, overwrite = TRUE, compress = "xz")
usethis::use_data(latent_params_sim, overwrite = TRUE, compress = "xz")

saveRDS(sim_internals, "data-raw/sim_internals.rds")
saveRDS(target_sim, file = "data-raw/target_sim.rds")

# 2. Probit link simulation (shares identical locations_sim, populations_sim, and latent_params_sim)
latent_probit <- generate_latent_current(setup, link = "probit")
sim_data_probit <- simulate_observations_from_latent(
  setup,
  latent_probit,
  uncensored = FALSE
)

observations_sim_probit <- sim_data_probit$observations_sim
sim_internals_probit <- sim_data_probit$sim_internals
target_sim_probit <- sim_data_probit$target_sim

usethis::use_data(observations_sim_probit, overwrite = TRUE, compress = "xz")

saveRDS(sim_internals_probit, "data-raw/sim_internals_probit.rds")
saveRDS(target_sim_probit, file = "data-raw/target_sim_probit.rds")

cat("Package data objects for logit and probit links updated successfully.\n")
