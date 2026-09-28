# Part of the package-data pipeline: build leave-recent-out (temporal forecast) holdout artifacts.
#
# The most recent observations correspond strictly to everything with the maximum
# calendar observation year: max(cohort + age).
#
# Evaluates LOO-PSIS approximation and exact MCMC refitting for holding out these
# frontier observations, evaluating out-of-sample forecast accuracy,
# CRPS sharpness and calibration via {scoringRules}, and exact ELPD vs PSIS-LOO.
#
# Exports data/leave_recent_out.rda.

pkgload::load_all(quiet = TRUE)
library(data.table)

cat(
  "=== 1. Grouped LOO-PSIS for Most Recent Observations: max(cohort + age) ===\n"
)
obs_meta <- canonicalize_observations(observations_sim, drop_extra = FALSE)
loc_meta <- canonicalize_locations(locations_sim)
obs_meta <- merge(
  obs_meta,
  loc_meta[, .(loc_id, layer)],
  by = "loc_id",
  all.x = TRUE
)
setorder(obs_meta, obs_c_id)

# Observation year is cohort + age
max_obs_year <- max(populations_sim$cohort + populations_sim$age)
cat(sprintf("Maximum observation year max(cohort + age): %d\n", max_obs_year))

recent_pop_meta <- populations_sim[cohort + age == max_obs_year]
recent_obs_ids <- unique(recent_pop_meta$obs_id)
recent_cols <- which(obs_meta$obs_id %in% recent_obs_ids)

cat(sprintf(
  "Total held-out most recent observations: %d\n",
  length(recent_cols)
))
cat("Breakdown by stream / layer:\n")
print(recent_pop_meta[,
  .(
    n_obs = uniqueN(obs_id),
    cohort = unique(cohort),
    age = unique(age),
    loc_count = uniqueN(loc_id)
  ),
  by = .(
    loc_id = ifelse(
      loc_id == "State",
      "State",
      ifelse(
        loc_id %in% c("Scruggs", "Simone", "Watson"),
        "Counties",
        "Schools"
      )
    )
  )
])

ll <- log_lik(fit_sim)
recent_ll <- matrix(rowSums(ll[, recent_cols, drop = FALSE]), ncol = 1)
colnames(recent_ll) <- "Most_Recent_Obs"

loo_recent <- loo::loo(recent_ll, r_eff = 1)
print(loo_recent)

pareto_k_val <- loo_recent$diagnostics$pareto_k[1]
psis_elpd_val <- loo_recent$pointwise[1, "elpd_loo"]
cat(sprintf("Pareto k for most recent observations: %.4f\n", pareto_k_val))
cat(sprintf("PSIS-LOO ELPD:                        %.2f\n", psis_elpd_val))

# --- 2. Fit Training Model (Omitting max(cohort + age) observations) --------
cat("\n=== 2. Fitting Training Model Without Most Recent Observations ===\n")
train_obs_early <- observations_sim[!obs_id %in% recent_obs_ids]
train_pops_early <- populations_sim[!obs_id %in% recent_obs_ids]

st_opts <- stan_options(
  iter = 1000L,
  chains = 4L,
  threading = TRUE,
  refresh = 0L,
  seed = 42L
)

imugap_opts <- imugap_options(
  max_age = 18L,
  max_cohort = 30L
)

t0 <- proc.time()
fit_early <- sampling(
  observations = train_obs_early,
  populations = train_pops_early,
  locations = locations_sim,
  imugap_opts = imugap_opts,
  stan_opts = st_opts
)
t1 <- proc.time()
cat(sprintf(
  "Sampling time for temporal holdout fit: %.2fs\n",
  (t1 - t0)["elapsed"]
))

# --- 3. Out-of-Sample Forecasting on Held-Out Observations ------------------
cat("\n=== 3. Predicting Held-Out Recent Observations ===\n")
recent_target_dt <- unique(recent_pop_meta[, .(loc_id, cohort, age, dose)])
target_grid_recent <- canonicalize_target(recent_target_dt, fit_early)

pred_recent_obj <- predict(
  fit_early,
  target = target_grid_recent,
  posterior_size = 200L
)
pred_recent_summary <- summary(pred_recent_obj)

# Empirical evaluation against held-out observations
obs_recent <- observations_sim[obs_id %in% recent_obs_ids]
obs_recent_empirical <- merge(
  recent_pop_meta[, .(obs_id, loc_id, cohort, age, dose, weight)],
  obs_recent[, .(obs_id, sample_n, positive, censored)],
  by = "obs_id"
)
obs_recent_empirical[, empirical_coverage := positive / sample_n]

eval_recent <- merge(
  pred_recent_summary,
  obs_recent_empirical[, .(
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
eval_recent[, `:=`(
  abs_error = abs(q50 - empirical_coverage),
  sq_error = (mean - empirical_coverage)^2,
  in_ci_95 = (empirical_coverage >= q2_5 & empirical_coverage <= q97_5)
)]

# --- 4. Proper Scoring Rules: CRPS Calculation -----------------------------
cat("\n=== 4. Computing Continuous Ranked Probability Score (CRPS) ===\n")
draws_recent <- pred_recent_obj$draws
n_draws <- dim(draws_recent)[1] * dim(draws_recent)[2]

crps_vals <- numeric(nrow(eval_recent))
ll_draws_recent <- matrix(0, nrow = n_draws, ncol = nrow(eval_recent))

for (j in seq_len(nrow(eval_recent))) {
  c_j <- eval_recent$cohort[j]
  a_j <- eval_recent$age[j]
  d_j <- eval_recent$dose[j]
  y_j <- eval_recent$positive[j]
  n_j <- eval_recent$sample_n[j]
  cens_j <- eval_recent$censored[j]
  y_prop <- eval_recent$empirical_coverage[j]

  t_idx <- which(
    pred_recent_summary$loc_id == eval_recent$loc_id[j] &
      pred_recent_summary$cohort == c_j &
      pred_recent_summary$age == a_j &
      pred_recent_summary$dose == d_j
  )
  p_draws <- as.vector(draws_recent[,, t_idx])
  crps_vals[j] <- scoringRules::crps_sample(y = y_prop, dat = p_draws)

  if (is.na(cens_j)) {
    ll_draws_recent[, j] <- dbinom(y_j, size = n_j, prob = p_draws, log = TRUE)
  } else {
    ll_draws_recent[, j] <- pbinom(
      y_j,
      size = n_j,
      prob = p_draws,
      log.p = TRUE
    )
  }
}
eval_recent[, crps := crps_vals]

# Helper function for numerically stable log-sum-exp
log_sum_exp <- function(x) {
  m <- max(x)
  m + log(sum(exp(x - m)))
}

exact_elpd_val <- log_sum_exp(rowSums(ll_draws_recent)) - log(n_draws)

elpd_comparison <- data.table(
  holdout = "Most Recent Observations (cohort + age == 33)",
  n_obs = nrow(eval_recent),
  pareto_k = round(pareto_k_val, 3),
  elpd_psis = round(psis_elpd_val, 2),
  elpd_exact = round(exact_elpd_val, 2),
  delta_elpd = round(psis_elpd_val - exact_elpd_val, 2)
)

metrics <- list(
  mae = mean(eval_recent$abs_error, na.rm = TRUE),
  rmse = sqrt(mean(eval_recent$sq_error, na.rm = TRUE)),
  coverage_95 = mean(eval_recent$in_ci_95, na.rm = TRUE),
  mean_crps = mean(eval_recent$crps, na.rm = TRUE),
  n_eval_points = nrow(eval_recent),
  max_obs_year = max_obs_year
)

leave_recent_out <- list(
  loo_res = loo_recent,
  elpd_comparison = elpd_comparison,
  metrics = metrics,
  predictions = pred_recent_summary,
  eval_comparison = eval_recent,
  draws = draws_recent
)

usethis::use_data(leave_recent_out, overwrite = TRUE, compress = "xz")
cat("Successfully generated and saved 'leave_recent_out'.\n")
