# Part of the package-data pipeline: fit a SINGLE leave-stream-out fold.
#
# Given a stream name or index (1..4), fits the imuGAP Stan model omitting that
# surveillance stream from observations and populations, predicts out-of-sample
# coverage on the (loc_id, cohort, age, dose) coordinates measured in that stream,
# and saves intermediate results to data-raw/scratch_stream_cv/fold_<name>.rds.
#
# Streams:
#   1: childvaxview (State level, ages 2 & 3, dose 1)
#   2: schoolvaxview (State level aggregate kindergarten, age 5, dose 2)
#   3: teenvaxview (State level adolescent surveys, ages 14..18, doses 1 & 2)
#   4: grade6 (County level 6th grade surveys, age 11, dose 2, censored)
#
# Usage:
#   Rscript data-raw/fit_single_stream_out.R --stream childvaxview
#   Rscript data-raw/fit_single_stream_out.R --index 1

pkgload::load_all(quiet = TRUE)
library(data.table)

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NULL) {
  idx <- which(args == flag)
  if (length(idx) > 0L && idx < length(args)) args[idx + 1L] else default
}

stream_names <- c("childvaxview", "schoolvaxview", "teenvaxview", "grade6")
n_streams <- length(stream_names)

target_index <- if (!is.null(get_arg("--index"))) {
  as.integer(get_arg("--index"))
} else {
  NA_integer_
}
target_stream <- get_arg("--stream")

if (
  !is.na(target_index) &&
    length(target_index) == 1L &&
    target_index >= 1L &&
    target_index <= n_streams
) {
  target_stream <- stream_names[target_index]
} else if (!is.null(target_stream) && target_stream %in% stream_names) {
  target_index <- match(target_stream, stream_names)
} else {
  stop(sprintf(
    "Please provide valid --index (1..%d) or --stream (%s)",
    n_streams,
    paste(stream_names, collapse = ", ")
  ))
}

cat(sprintf(
  "=== [Stream Fold %d/%d] Fitting without stream: '%s' ===\n",
  target_index,
  n_streams,
  target_stream
))

# 1. Identify observation IDs belonging to the target stream
omitted_obs_ids <- switch(
  target_stream,
  childvaxview = populations_sim[loc_id == "State" & age <= 3L, unique(obs_id)],
  schoolvaxview = populations_sim[
    loc_id == "State" & age == 5L,
    unique(obs_id)
  ],
  teenvaxview = populations_sim[loc_id == "State" & age >= 14L, unique(obs_id)],
  grade6 = populations_sim[
    loc_id %in% c("Scruggs", "Simone", "Watson"),
    unique(obs_id)
  ]
)

train_obs <- observations_sim[!obs_id %in% omitted_obs_ids]
train_pops <- populations_sim[!obs_id %in% omitted_obs_ids]

# 2. Fit model with max_age = 18 and max_cohort = 30 to support full age horizon
st_opts <- stan_options(
  iter = 1000L,
  chains = 4L,
  threading = TRUE,
  refresh = 0L,
  seed = 42L + target_index
)

imugap_opts <- imugap_options(
  max_age = 18L,
  max_cohort = 30L
)

t0 <- proc.time()
fit <- sampling(
  observations = train_obs,
  populations = train_pops,
  locations = locations_sim,
  imugap_opts = imugap_opts,
  stan_opts = st_opts
)
t1 <- proc.time()
cat(sprintf(
  "Fold %d ('%s') sampling wall time: %.2fs\n",
  target_index,
  target_stream,
  (t1 - t0)["elapsed"]
))

# 3. Target grid restricted strictly to the (loc_id, cohort, age, dose) in this stream
stream_obs_meta <- populations_sim[obs_id %in% omitted_obs_ids]
stream_target_dt <- unique(stream_obs_meta[, .(loc_id, cohort, age, dose)])
target_grid <- canonicalize_target(stream_target_dt, fit)

# 4. Out-of-sample prediction on the exact measurement coordinates
pred <- predict(object = fit, target = target_grid, posterior_size = 200L)
pred_summary <- summary(pred)
pred_summary[, `:=`(heldout_stream = target_stream, fold_index = target_index)]

# 5. Write fold output to scratch directory
out_dir <- "data-raw/scratch_stream_cv"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_file <- file.path(out_dir, sprintf("fold_%s.rds", target_stream))

saveRDS(
  list(
    index = target_index,
    stream = target_stream,
    omitted_obs_ids = omitted_obs_ids,
    summary = pred_summary,
    draws = pred$draws
  ),
  file = out_file
)

cat(sprintf("Fold %d output written to '%s'.\n", target_index, out_file))
