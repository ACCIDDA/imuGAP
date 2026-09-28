# Part of the package-data pipeline: consolidate all stream fold outputs into leave_stream_out.
#
# Gathers intermediate fold results from data-raw/scratch_stream_cv/fold_*.rds,
# evaluates grouped LOO-PSIS diagnostics, evaluates out-of-sample predictions
# against empirical measurements, computes exact out-of-sample ELPD vs PSIS-LOO, and
# writes the consolidated package data object data/leave_stream_out.rda.

pkgload::load_all(quiet = TRUE)
library(data.table)

in_dir <- "data-raw/scratch_stream_cv"
stream_names <- c("childvaxview", "schoolvaxview", "teenvaxview", "grade6")
n_expected <- length(stream_names)

fold_files <- file.path(in_dir, sprintf("fold_%s.rds", stream_names))
missing_files <- fold_files[!file.exists(fold_files)]

if (length(missing_files) > 0L) {
  stop(sprintf(
    "Expected %d fold files in '%s', missing: %s. Run all stream folds before consolidating.",
    n_expected,
    in_dir,
    paste(basename(missing_files), collapse = ", ")
  ))
}

cat(sprintf(
  "Consolidating %d stream fold files from '%s'...\n",
  length(fold_files),
  in_dir
))
folds <- lapply(fold_files, readRDS)
names(folds) <- stream_names

# 1. Grouped LOO-PSIS for the 4 streams on the full fit
obs_meta <- canonicalize_observations(observations_sim, drop_extra = FALSE)
loc_meta <- canonicalize_locations(locations_sim)
obs_meta <- merge(
  obs_meta,
  loc_meta[, .(loc_id, layer)],
  by = "loc_id",
  all.x = TRUE
)
setorder(obs_meta, obs_c_id)

ll <- log_lik(fit_sim)

stream_cols_list <- list(
  childvaxview = which(obs_meta$loc_id == "State" & obs_meta$age_min <= 3L),
  schoolvaxview = which(obs_meta$loc_id == "State" & obs_meta$age_min == 5L),
  teenvaxview = which(obs_meta$loc_id == "State" & obs_meta$age_min >= 14L),
  grade6 = which(obs_meta$layer == 2L)
)

stream_ll <- do.call(
  cbind,
  lapply(stream_cols_list, function(cols) {
    rowSums(ll[, cols, drop = FALSE])
  })
)

loo_stream <- loo::loo(stream_ll, r_eff = rep(1, ncol(stream_ll)))
print(loo_stream)

pareto_k_vec <- loo_stream$diagnostics$pareto_k
names(pareto_k_vec) <- stream_names
psis_elpd_vec <- loo_stream$pointwise[, "elpd_loo"]
names(psis_elpd_vec) <- stream_names

# 2. Combine out-of-sample prediction summaries and draws
pred_oos_combined <- rbindlist(lapply(folds, `[[`, "summary"))
oos_draws <- setNames(
  lapply(folds, `[[`, "draws"),
  stream_names
)

# 3. Align out-of-sample predictions with empirical measurements
eval_list <- list()
exact_elpd_vec <- numeric(length(stream_names))
names(exact_elpd_vec) <- stream_names

log_sum_exp <- function(x) {
  m <- max(x)
  m + log(sum(exp(x - m)))
}

for (st in stream_names) {
  fold_st <- folds[[st]]
  st_meta <- populations_sim[obs_id %in% fold_st$omitted_obs_ids]
  st_obs <- observations_sim[obs_id %in% fold_st$omitted_obs_ids]

  st_empirical <- merge(
    st_meta[, .(obs_id, loc_id, cohort, age, dose, weight)],
    st_obs[, .(obs_id, sample_n, positive, censored)],
    by = "obs_id"
  )
  st_empirical[, empirical_coverage := positive / sample_n]

  st_eval <- merge(
    fold_st$summary,
    st_empirical[, .(
      loc_id,
      cohort,
      age,
      dose,
      obs_id,
      sample_n,
      positive,
      censored,
      empirical_coverage
    )],
    by = c("loc_id", "cohort", "age", "dose")
  )
  st_eval[, `:=`(
    abs_error = abs(q50 - empirical_coverage),
    sq_error = (mean - empirical_coverage)^2,
    in_ci_95 = (empirical_coverage >= q2_5 & empirical_coverage <= q97_5)
  )]
  eval_list[[st]] <- st_eval

  # Exact out-of-sample ELPD calculation
  draws_st <- fold_st$draws
  n_draws <- dim(draws_st)[1] * dim(draws_st)[2]
  ll_draws_st <- matrix(0, nrow = n_draws, ncol = nrow(st_eval))

  for (j in seq_len(nrow(st_eval))) {
    c_j <- st_eval$cohort[j]
    a_j <- st_eval$age[j]
    d_j <- st_eval$dose[j]
    y_j <- st_eval$positive[j]
    n_j <- st_eval$sample_n[j]
    cens_j <- st_eval$censored[j]

    t_idx <- which(
      fold_st$summary$loc_id == st_eval$loc_id[j] &
        fold_st$summary$cohort == c_j &
        fold_st$summary$age == a_j &
        fold_st$summary$dose == d_j
    )
    p_draws <- as.vector(draws_st[,, t_idx])
    if (is.na(cens_j)) {
      ll_draws_st[, j] <- dbinom(y_j, size = n_j, prob = p_draws, log = TRUE)
    } else {
      ll_draws_st[, j] <- pbinom(y_j, size = n_j, prob = p_draws, log.p = TRUE)
    }
  }
  exact_elpd_vec[st] <- log_sum_exp(rowSums(ll_draws_st)) - log(n_draws)
}

eval_comparison <- rbindlist(eval_list)

# 4. Summary metrics
metrics <- list(
  by_stream = lapply(eval_list, function(df) {
    list(
      mae = mean(df$abs_error, na.rm = TRUE),
      rmse = sqrt(mean(df$sq_error, na.rm = TRUE)),
      coverage_95 = mean(df$in_ci_95, na.rm = TRUE),
      n_eval_points = nrow(df)
    )
  }),
  overall = list(
    mae = mean(eval_comparison$abs_error, na.rm = TRUE),
    rmse = sqrt(mean(eval_comparison$sq_error, na.rm = TRUE)),
    coverage_95 = mean(eval_comparison$in_ci_95, na.rm = TRUE),
    n_eval_points = nrow(eval_comparison)
  )
)

# 5. ELPD comparison
elpd_comparison <- data.table(
  stream = c(
    "ChildVaxView (State)",
    "SchoolVaxView (State)",
    "TeenVaxView (State)",
    "Grade 6 (County)"
  ),
  stream_code = stream_names,
  n_obs = vapply(stream_cols_list, length, integer(1)),
  pareto_k = round(pareto_k_vec[stream_names], 3),
  elpd_psis = round(psis_elpd_vec[stream_names], 2),
  elpd_exact = round(exact_elpd_vec[stream_names], 2),
  delta_elpd = round(
    psis_elpd_vec[stream_names] - exact_elpd_vec[stream_names],
    2
  )
)

# 6. Export consolidated package dataset
leave_stream_out <- list(
  loo_res = loo_stream,
  elpd_comparison = elpd_comparison,
  metrics = metrics,
  predictions = pred_oos_combined,
  eval_comparison = eval_comparison,
  draws = oos_draws,
  streams = stream_names
)

usethis::use_data(leave_stream_out, overwrite = TRUE, compress = "xz")
cat("Successfully generated and saved 'leave_stream_out'.\n")
