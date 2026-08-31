# Part A of the package-data pipeline: simulate the *_sim inputs and the static
# latent-parameter fixture.
#
# This step depends on the private nc_measles dataset (read below) and so cannot
# run in CI; the resulting *_sim inputs and latent_params_sim are tracked in git.
# It also writes data-raw/sim_internals.rds, consumed by Part B
# (data-raw/fit_data.R) to build the genuinely fit-derived artifacts
# (fit_sim/target_sim/predict_sim) without re-running this simulation.
# latent_params_sim used to live in Part B, but its parameters and its
# (analytic, fit-free) coverage are simulation properties, not fit outputs, so
# it moved here as tracked static data (#105). Run with `just data` (or
# `just data-inputs` for this step alone).

# Load only the packages this script actually uses. If you attach e.g. the
# full tidyverse, you'll pull in lubridate; lubridate then gets captured in the
# fitted model's `@.MISC` environment and baked into data/fit_sim.rda, tripping
# R CMD check's "namespace references in data files".
library(data.table)

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_code()
} else {
  stop("pkgload not found")
}

# Source simulation helper functions
source("data-raw/dataset_helpers.R")

# Run simulation pipeline under current latent model
setup <- get_simulation_setup(seed = 93254)
latent <- generate_latent_current(setup)
sim_data <- simulate_observations_from_latent(setup, latent, obs_seed = 93254)

observations_sim <- sim_data$observations_sim
populations_sim <- sim_data$populations_sim
locations_sim <- sim_data$locations_sim
latent_params_sim <- sim_data$latent_params_sim
sim_internals <- sim_data$sim_internals
target_sim <- sim_data$target_sim

# Create imugap input package data objects
usethis::use_data(observations_sim, overwrite = TRUE)
usethis::use_data(populations_sim, overwrite = TRUE)
usethis::use_data(locations_sim, overwrite = TRUE)
usethis::use_data(latent_params_sim, overwrite = TRUE)

saveRDS(sim_internals, "data-raw/sim_internals.rds")
saveRDS(target_sim, file = "data-raw/target_sim.rds")
cat("Package data objects updated successfully.\n")
