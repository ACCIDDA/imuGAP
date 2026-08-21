# Create reference example data sets for state-county-school hierarchical structure.
#
# Generates reference data under 4 alternative approaches:
#  1. Odds-Ratio (OR) Balanced (Current approach)
#  2. Odds-Ratio (OR) Unbalanced
#  3. Logit Offset Unbalanced
#  4. Logit Offset Balanced + Moving Offset Parameter
#
# Shared code and simulation helpers are loaded from dataset_helpers.R.

library(data.table)

# Load package code using pkgload
if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_code()
} else {
  stop("pkgload not found")
}

# Source dataset helper functions
source("data-raw/dataset_helpers.R")

# Run full simulation pipeline for all 4 approaches
results <- generate_all_reference_datasets(seed = 93254)
setup <- results$setup
datasets <- results$datasets

# Save outputs for each approach to data-raw/ for side-by-side evaluation
dir.create("data-raw", showWarnings = FALSE)
for (name in names(datasets)) {
  saveRDS(datasets[[name]], file = sprintf("data-raw/sim_%s.rds", name))
  cat(sprintf("Saved data-raw/sim_%s.rds\n", name))
}

# Standard package data defaults to Approach 1 (or_balanced) for backwards compatibility
canonical_ds <- datasets$or_balanced

observations_sim <- canonical_ds$observations_sim
populations_sim <- canonical_ds$populations_sim
locations_sim <- canonical_ds$locations_sim
latent_params_sim <- canonical_ds$latent_params_sim
sim_internals <- canonical_ds$sim_internals
target_sim <- canonical_ds$target_sim

# Create imugap input package data objects
usethis::use_data(observations_sim, overwrite = TRUE)
usethis::use_data(populations_sim, overwrite = TRUE)
usethis::use_data(locations_sim, overwrite = TRUE)
usethis::use_data(latent_params_sim, overwrite = TRUE)

saveRDS(sim_internals, "data-raw/sim_internals.rds")
saveRDS(target_sim, file = "data-raw/target_sim.rds")

cat("Package data objects updated successfully.\n")
