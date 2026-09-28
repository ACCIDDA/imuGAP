# Part of the package-data pipeline: build leave-stream-out holdout artifacts.
#
# Evaluates LOO-PSIS approximation and exact MCMC refitting for leaving out:
#  1. State-level surveillance stream (ChildVaxView & TeenVaxView)
#  2. County-level surveillance stream (Grade 6 surveys)
#
# Exports data/leave_stream_out.rda.

pkgload::load_all(quiet = TRUE)
library(data.table)

cat("=== 1. Grouped LOO-PSIS for Surveillance Streams ===\n")
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

state_cols <- which(obs_meta$layer == 1L)
county_cols <- which(obs_meta$layer == 2L)

stream_ll <- cbind(
  State = rowSums(ll[, state_cols, drop = FALSE]),
  County = rowSums(ll[, county_cols, drop = FALSE])
)

loo_stream <- loo::loo(stream_ll, r_eff = rep(1, ncol(stream_ll)))
print(loo_stream)
pareto_k_stream <- loo_stream$diagnostics$pareto_k
names(pareto_k_stream) <- c("State", "County")
psis_elpd_stream <- loo_stream$pointwise[, "elpd_loo"]
names(psis_elpd_stream) <- c("State", "County")

cat(sprintf("State Stream Pareto k:  %.4f\n", pareto_k_stream["State"]))
cat(sprintf("County Stream Pareto k: %.4f\n", pareto_k_stream["County"]))

st_opts <- stan_options(
  iter = 1000L,
  chains = 4L,
  threading = TRUE,
  refresh = 250L,
  seed = 42L
)

# Helper function for numerically stable log-sum-exp
log_sum_exp <- function(x) {
  m <- max(x)
  m + log(sum(exp(x - m)))
}

# --- Stream 1: Leave Out State Stream --------------------------------------
cat("\n=== 2. Fitting Model Without State Stream ===\n")
state_obs_ids <- unique(populations_sim[loc_id == "State", obs_id])
train_obs_no_state <- observations_sim[!obs_id %in% state_obs_ids]
train_pops_no_state <- populations_sim[!obs_id %in% state_obs_ids]

t0 <- proc.time()
fit_no_state <- sampling(
  observations = train_obs_no_state,
  populations = train_pops_no_state,
  locations = locations_sim,
  stan_opts = st_opts
)
t1 <- proc.time()
cat(sprintf(
  "Sampling time without State stream: %.2fs\n",
  (t1 - t0)["elapsed"]
))

# Predict out-of-sample on State stream coordinates
state_pop_meta <- populations_sim[loc_id == "State"]
state_target_dt <- unique(state_pop_meta[, .(loc_id, cohort, age, dose)])
target_grid_state <- canonicalize_target(state_target_dt, fit_no_state)

pred_state_obj <- predict(
  fit_no_state,
  target = target_grid_state,
  posterior_size = 200L
)
pred_state_summary <- summary(pred_state_obj)
pred_state_summary[, heldout_stream := "State"]

# Empirical evaluation against held-out observations
obs_state <- observations_sim[obs_id %in% state_obs_ids]
obs_state_empirical <- merge(
  state_pop_meta[, .(obs_id, loc_id, cohort, age, dose, weight)],
  obs_state[, .(obs_id, sample_n, positive, censored)],
  by = "obs_id"
)
obs_state_empirical[, empirical_coverage := positive / sample_n]

eval_state <- merge(
  pred_state_summary,
  obs_state_empirical[, .(
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
eval_state[, `:=`(
  abs_error = abs(q50 - empirical_coverage),
  sq_error = (mean - empirical_coverage)^2,
  in_ci_95 = (empirical_coverage >= q2_5 & empirical_coverage <= q97_5)
)]

# Exact out-of-sample ELPD for State stream
draws_state <- pred_state_obj$draws
n_draws <- dim(draws_state)[1] * dim(draws_state)[2]
ll_draws_state <- matrix(0, nrow = n_draws, ncol = nrow(eval_state))
for (j in seq_len(nrow(eval_state))) {
  c_j <- eval_state$cohort[j]
  a_j <- eval_state$age[j]
  d_j <- eval_state$dose[j]
  y_j <- eval_state$positive[j]
  n_j <- eval_state$sample_n[j]
  cens_j <- eval_state$censored[j]

  t_idx <- which(
    pred_state_summary$cohort == c_j &
      pred_state_summary$age == a_j &
      pred_state_summary$dose == d_j
  )
  p_draws <- as.vector(draws_state[,, t_idx])
  if (is.na(cens_j)) {
    ll_draws_state[, j] <- dbinom(y_j, size = n_j, prob = p_draws, log = TRUE)
  } else {
    ll_draws_state[, j] <- pbinom(y_j, size = n_j, prob = p_draws, log.p = TRUE)
  }
}
exact_elpd_state <- log_sum_exp(rowSums(ll_draws_state)) - log(n_draws)

# --- Stream 2: Leave Out County Stream -------------------------------------
cat("\n=== 3. Fitting Model Without County Stream ===\n")
county_names <- locations_sim[parent_id == "State", loc_id]
county_obs_ids <- unique(populations_sim[loc_id %in% county_names, obs_id])
train_obs_no_county <- observations_sim[!obs_id %in% county_obs_ids]
train_pops_no_county <- populations_sim[!obs_id %in% county_obs_ids]

t0 <- proc.time()
fit_no_county <- sampling(
  observations = train_obs_no_county,
  populations = train_pops_no_county,
  locations = locations_sim,
  stan_opts = st_opts
)
t1 <- proc.time()
cat(sprintf(
  "Sampling time without County stream: %.2fs\n",
  (t1 - t0)["elapsed"]
))

# Predict out-of-sample on County stream coordinates
county_pop_meta <- populations_sim[loc_id %in% county_names]
county_target_dt <- unique(county_pop_meta[, .(loc_id, cohort, age, dose)])
target_grid_county <- canonicalize_target(county_target_dt, fit_no_county)

pred_county_obj <- predict(
  fit_no_county,
  target = target_grid_county,
  posterior_size = 200L
)
pred_county_summary <- summary(pred_county_obj)
pred_county_summary[, heldout_stream := "County"]

# Empirical evaluation against held-out observations
obs_county <- observations_sim[obs_id %in% county_obs_ids]
obs_county_empirical <- merge(
  county_pop_meta[, .(obs_id, loc_id, cohort, age, dose, weight)],
  obs_county[, .(obs_id, sample_n, positive, censored)],
  by = "obs_id"
)
obs_county_empirical[, empirical_coverage := positive / sample_n]

eval_county <- merge(
  pred_county_summary,
  obs_county_empirical[, .(
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
eval_county[, `:=`(
  abs_error = abs(q50 - empirical_coverage),
  sq_error = (mean - empirical_coverage)^2,
  in_ci_95 = (empirical_coverage >= q2_5 & empirical_coverage <= q97_5)
)]

# Exact out-of-sample ELPD for County stream
draws_county <- pred_county_obj$draws
ll_draws_county <- matrix(0, nrow = n_draws, ncol = nrow(eval_county))
for (j in seq_len(nrow(eval_county))) {
  c_j <- eval_county$cohort[j]
  a_j <- eval_county$age[j]
  d_j <- eval_county$dose[j]
  y_j <- eval_county$positive[j]
  n_j <- eval_county$sample_n[j]
  cens_j <- eval_county$censored[j]

  t_idx <- which(
    pred_county_summary$loc_id == eval_county$loc_id[j] &
      pred_county_summary$cohort == c_j &
      pred_county_summary$age == a_j &
      pred_county_summary$dose == d_j
  )
  p_draws <- as.vector(draws_county[,, t_idx])
  if (is.na(cens_j)) {
    ll_draws_county[, j] <- dbinom(y_j, size = n_j, prob = p_draws, log = TRUE)
  } else {
    ll_draws_county[, j] <- pbinom(
      y_j,
      size = n_j,
      prob = p_draws,
      log.p = TRUE
    )
  }
}
exact_elpd_county <- log_sum_exp(rowSums(ll_draws_county)) - log(n_draws)

# --- 4. Consolidation & Comparison ----------------------------------------
elpd_comparison <- data.table(
  stream = c("State Surveys", "County Surveys (Censored)"),
  stream_code = c("State", "County"),
  pareto_k = c(pareto_k_stream["State"], pareto_k_stream["County"]),
  elpd_psis = c(psis_elpd_stream["State"], psis_elpd_stream["County"]),
  elpd_exact = c(exact_elpd_state, exact_elpd_county)
)
elpd_comparison[, delta_elpd := elpd_psis - elpd_exact]

metrics <- list(
  state = list(
    mae = mean(eval_state$abs_error, na.rm = TRUE),
    rmse = sqrt(mean(eval_state$sq_error, na.rm = TRUE)),
    coverage_95 = mean(eval_state$in_ci_95, na.rm = TRUE),
    n_eval_points = nrow(eval_state)
  ),
  county = list(
    mae = mean(eval_county$abs_error, na.rm = TRUE),
    rmse = sqrt(mean(eval_county$sq_error, na.rm = TRUE)),
    coverage_95 = mean(eval_county$in_ci_95, na.rm = TRUE),
    n_eval_points = nrow(eval_county)
  )
)

leave_stream_out <- list(
  loo_res = loo_stream,
  elpd_comparison = elpd_comparison,
  metrics = metrics,
  predictions = rbind(pred_state_summary, pred_county_summary),
  eval_comparison = rbind(eval_state, eval_county),
  draws = list(
    State = draws_state,
    County = draws_county
  )
)

usethis::use_data(leave_stream_out, overwrite = TRUE, compress = "xz")
cat("\nSuccessfully generated and saved 'leave_stream_out'.\n")
