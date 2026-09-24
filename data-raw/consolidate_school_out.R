# Part of the package-data pipeline: consolidate all fold outputs into leave_school_out_scruggs.
#
# Gathers intermediate fold results from data-raw/scratch_school_cv/fold_*.rds,
# evaluates out-of-sample predictions against empirical measurements,
# computes shrinkage metrics relative to full-model in-sample fits, and
# writes the consolidated package data object data/leave_school_out_scruggs.rda.

pkgload::load_all(quiet = TRUE)
library(data.table)

in_dir <- "data-raw/scratch_school_cv"
fold_files <- sort(list.files(
  in_dir,
  pattern = "^fold_.*\\.rds$",
  full.names = TRUE
))
scruggs_schools <- locations_sim[parent_id == "Scruggs", loc_id]
n_expected <- length(scruggs_schools)

if (length(fold_files) < n_expected) {
  stop(sprintf(
    "Expected %d fold files in '%s', found %d. Run all folds before consolidating.",
    n_expected,
    in_dir,
    length(fold_files)
  ))
}

cat(sprintf(
  "Consolidating %d fold files from '%s'...\n",
  length(fold_files),
  in_dir
))
folds <- lapply(fold_files, readRDS)

# 1. Combine out-of-sample prediction summaries and draws
pred_oos_combined <- rbindlist(lapply(folds, `[[`, "summary"))
oos_draws <- setNames(
  lapply(folds, `[[`, "draws"),
  vapply(folds, `[[`, "school", FUN.VALUE = character(1))
)

# 2. Extract empirical measurements for Scruggs schools from population metadata
school_obs_meta <- populations_sim[loc_id %in% scruggs_schools]
obs_scruggs <- observations_sim[obs_id %in% school_obs_meta$obs_id]

obs_empirical <- merge(
  school_obs_meta[, .(obs_id, loc_id, cohort, age, dose, weight)],
  obs_scruggs[, .(obs_id, sample_n, positive)],
  by = "obs_id"
)
obs_empirical[, empirical_coverage := positive / sample_n]

# Align out-of-sample predictions with empirical observations
eval_comparison <- merge(
  pred_oos_combined,
  obs_empirical[, .(
    loc_id,
    cohort,
    age,
    dose,
    obs_id,
    sample_n,
    positive,
    empirical_coverage
  )],
  by = c("loc_id", "cohort", "age", "dose")
)

# Compute point errors and interval coverage indicators
eval_comparison[, `:=`(
  abs_error = abs(q50 - empirical_coverage),
  sq_error = (mean - empirical_coverage)^2,
  in_ci_95 = (empirical_coverage >= q2_5 & empirical_coverage <= q97_5)
)]

# Summary metrics across all held-out schools
metrics <- list(
  mae = mean(eval_comparison$abs_error, na.rm = TRUE),
  rmse = sqrt(mean(eval_comparison$sq_error, na.rm = TRUE)),
  coverage_95 = mean(eval_comparison$in_ci_95, na.rm = TRUE),
  n_eval_points = nrow(eval_comparison)
)

# 3. In-Sample vs. Out-of-Sample Shrinkage Comparison
data("fit_sim", package = "imuGAP")
target_all_scruggs <- canonicalize_target(
  eval_comparison[, .(loc_id, cohort, age, dose)],
  fit_sim
)
pred_insample_obj <- predict(
  fit_sim,
  target = target_all_scruggs,
  posterior_size = 200L
)
pred_insample <- summary(pred_insample_obj)

shrinkage_comp <- merge(
  eval_comparison[, .(
    loc_id,
    cohort,
    age,
    dose,
    empirical_coverage,
    oos_q50 = q50,
    oos_mean = mean
  )],
  pred_insample[, .(
    loc_id,
    cohort,
    age,
    dose,
    insample_q50 = q50,
    insample_mean = mean
  )],
  by = c("loc_id", "cohort", "age", "dose")
)
shrinkage_comp[, `:=`(
  shrinkage_delta = oos_mean - insample_mean
)]

# 4. Export consolidated package dataset
leave_school_out_scruggs <- list(
  predictions = pred_oos_combined,
  eval_comparison = eval_comparison,
  metrics = metrics,
  shrinkage_comparison = shrinkage_comp,
  draws = oos_draws,
  schools = scruggs_schools
)

usethis::use_data(leave_school_out_scruggs, overwrite = TRUE, compress = "xz")
cat("Successfully generated and saved 'leave_school_out_scruggs'.\n")
