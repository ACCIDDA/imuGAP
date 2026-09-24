# Part of the package-data pipeline: fit a SINGLE leave-school-out fold.
#
# Given a school index (1..10) or school name, fits the imuGAP Stan model
# omitting that school from observations and populations, predicts out-of-sample
# coverage strictly on the (cohort, age, dose) combinations measured for that school,
# and saves the intermediate result to data-raw/scratch_school_cv/fold_<index>.rds.
#
# Usage:
#   Rscript data-raw/fit_single_school_out.R --index 1
#   Rscript data-raw/fit_single_school_out.R --school "Chickadee Elementary"

pkgload::load_all(quiet = TRUE)
library(data.table)

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NULL) {
  idx <- which(args == flag)
  if (length(idx) > 0L && idx < length(args)) args[idx + 1L] else default
}

scruggs_schools <- locations_sim[parent_id == "Scruggs", loc_id]
n_schools <- length(scruggs_schools)

target_index <- as.integer(get_arg("--index"))
target_school <- get_arg("--school")

if (!is.na(target_index) && target_index >= 1L && target_index <= n_schools) {
  target_school <- scruggs_schools[target_index]
} else if (!is.null(target_school) && target_school %in% scruggs_schools) {
  target_index <- match(target_school, scruggs_schools)
} else {
  stop(sprintf(
    "Please provide valid --index (1..%d) or --school (e.g. '%s')",
    n_schools,
    scruggs_schools[1L]
  ))
}

cat(sprintf(
  "=== [Fold %d/%d] Fitting without school: '%s' ===\n",
  target_index,
  n_schools,
  target_school
))

# 1. Identify observation IDs and metadata belonging to the omitted school
school_obs_meta <- populations_sim[loc_id == target_school]
omitted_obs_ids <- unique(school_obs_meta$obs_id)

train_obs <- observations_sim[!obs_id %in% omitted_obs_ids]
train_pops <- populations_sim[!obs_id %in% omitted_obs_ids]

# 2. Fit model (preserving full location hierarchy for spatial shrinkage)
st_opts <- stan_options(
  iter = 1000L,
  chains = 4L,
  threading = TRUE,
  refresh = 0L,
  seed = 42L + target_index
)

t0 <- proc.time()
fit <- sampling(
  observations = train_obs,
  populations = train_pops,
  locations = locations_sim,
  stan_opts = st_opts
)
t1 <- proc.time()
cat(sprintf(
  "Fold %d sampling wall time: %.2fs\n",
  target_index,
  (t1 - t0)["elapsed"]
))

# 3. Target grid restricted strictly to the (cohort, age, dose) measured in this school
school_target_dt <- unique(school_obs_meta[, .(loc_id, cohort, age, dose)])
target_grid <- canonicalize_target(school_target_dt, fit)

# 4. Out-of-sample prediction on the exact measurement coordinates
pred <- predict(object = fit, target = target_grid, posterior_size = 200L)
pred_summary <- summary(pred)
pred_summary[, `:=`(heldout_school = target_school, fold_index = target_index)]

# 5. Write fold output to scratch directory
out_dir <- "data-raw/scratch_school_cv"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_file <- file.path(out_dir, sprintf("fold_%02d.rds", target_index))

saveRDS(
  list(
    index = target_index,
    school = target_school,
    summary = pred_summary,
    draws = pred$draws
  ),
  file = out_file
)

cat(sprintf("Fold %d output written to '%s'.\n", target_index, out_file))
