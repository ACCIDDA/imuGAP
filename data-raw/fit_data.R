# Part B of the package-data pipeline: build the fit-derived artifacts.
#
# Produces fit_sim, target_sim, and predict_sim from the tracked *_sim inputs,
# as well as 1-layer and 2-layer variants (fit_sim_1layer, predict_sim_1layer,
# fit_sim_2layer, predict_sim_2layer) for layer demonstration and vignettes.

pkgload::load_all(quiet = TRUE)
library(data.table)

measure_time <- function(expr) {
  t0 <- proc.time()
  val <- force(expr)
  t1 <- proc.time()
  list(val = val, time = t1 - t0)
}

print_time <- function(label, pt) {
  cat(sprintf(
    "  [%s Timing] user: %.2fs | system: %.2fs | wall: %.2fs\n",
    label,
    pt["user.self"] + pt["user.child"],
    pt["sys.self"] + pt["sys.child"],
    pt["elapsed"]
  ))
}

print_stan_summary <- function(stanfit) {
  sum_mat <- rstan::summary(stanfit)$summary
  rhat_col <- if ("Rhat" %in% colnames(sum_mat)) {
    "Rhat"
  } else if ("rhat" %in% colnames(sum_mat)) {
    "rhat"
  } else {
    NULL
  }
  neff_col <- if ("n_eff" %in% colnames(sum_mat)) {
    "n_eff"
  } else if ("ess_bulk" %in% colnames(sum_mat)) {
    "ess_bulk"
  } else {
    NULL
  }

  max_rhat <- if (!is.null(rhat_col)) {
    max(sum_mat[, rhat_col], na.rm = TRUE)
  } else {
    NA_real_
  }
  max_rhat_par <- if (!is.null(rhat_col)) {
    rownames(sum_mat)[which.max(sum_mat[, rhat_col])]
  } else {
    "NA"
  }
  min_neff <- if (!is.null(neff_col)) {
    min(sum_mat[, neff_col], na.rm = TRUE)
  } else {
    NA_real_
  }
  min_neff_par <- if (!is.null(neff_col)) {
    rownames(sum_mat)[which.min(sum_mat[, neff_col])]
  } else {
    "NA"
  }
  median_neff <- if (!is.null(neff_col)) {
    stats::median(sum_mat[, neff_col], na.rm = TRUE)
  } else {
    NA_real_
  }

  num_div <- tryCatch(rstan::get_num_divergent(stanfit), error = function(e) 0L)
  num_treedepth <- tryCatch(
    rstan::get_num_max_treedepth(stanfit),
    error = function(e) 0L
  )

  cat(sprintf(
    "  [Stan Diagnostics] Divergences: %d | Max treedepth: %d\n",
    num_div,
    num_treedepth
  ))
  cat(sprintf(
    "  [Convergence]      Max R-hat: %.4f (%s) | Min ESS: %.1f (%s) | Median ESS: %.1f\n",
    max_rhat,
    max_rhat_par,
    min_neff,
    min_neff_par,
    median_neff
  ))

  el_time <- tryCatch(rstan::get_elapsed_time(stanfit), error = function(e) {
    NULL
  })
  if (!is.null(el_time)) {
    cat("  [Chain Times]      Warmup (s) | Sampling (s) | Total (s)\n")
    for (ch in seq_len(nrow(el_time))) {
      cat(sprintf(
        "    Chain %d:          %8.2f | %12.2f | %9.2f\n",
        ch,
        el_time[ch, "warmup"],
        el_time[ch, "sample"],
        sum(el_time[ch, ])
      ))
    }
  }
}

st_opts <- stan_options(
  iter = 1000,
  chains = 4L,
  threading = TRUE,
  refresh = 250,
  seed = 1L
)

cat(
  "\n=== Initiating fit with stan spec: ===\n",
  toString(st_opts),
  "\n"
)

# --- 3-Layer Fit (State -> County -> School) -------------------------------
cat("\n=== 3-Layer Fit (State -> County -> School) ===\n")
fit_res <- measure_time(sampling(
  observations_sim,
  populations_sim,
  locations_sim,
  stan_opts = st_opts
))
fit_sim <- fit_res$val
print_time("Fitting", fit_res$time)
print_stan_summary(fit_sim$raw_fit)

stopifnot(
  inherits(fit_sim$raw_fit, "stanfit"),
  all(c("beta_bs", "lambda_raw") %in% fit_sim$raw_fit@model_pars),
  all(is.finite(rstan::extract(fit_sim$raw_fit, pars = "beta_bs")$beta_bs))
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

pred_res <- measure_time(
  predict(object = fit_sim, target = target_sim, posterior_size = 100)
)
predict_sim <- pred_res$val
print_time("Predicting", pred_res$time)
usethis::use_data(predict_sim, overwrite = TRUE, compress = "xz")

# --- 2-Layer Fit (State -> County) ----------------------------------------
cat("\n=== 2-Layer Fit (State -> County) ===\n")
locations_sim_2layer <- locations_sim[is.na(parent_id) | parent_id == "State"]

populations_sim_2layer <- copy(populations_sim)
loc_map_2layer <- locations_sim[!is.na(parent_id), .(loc_id, parent_id)]
populations_sim_2layer[loc_map_2layer, on = .(loc_id), loc_id := i.parent_id]
populations_sim_2layer <- populations_sim_2layer[,
  .(weight = sum(weight)),
  by = .(obs_id, loc_id, cohort, age, dose)
]
observations_sim_2layer <- copy(observations_sim)

fit_res_2layer <- measure_time(sampling(
  observations_sim_2layer,
  populations_sim_2layer,
  locations_sim_2layer,
  stan_opts = st_opts
))
fit_sim_2layer <- fit_res_2layer$val
print_time("Fitting", fit_res_2layer$time)
print_stan_summary(fit_sim_2layer$raw_fit)

stopifnot(inherits(fit_sim_2layer$raw_fit, "stanfit"))
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

pred_res_2layer <- measure_time(
  predict(
    object = fit_sim_2layer,
    target = target_sim_2layer,
    posterior_size = 100
  )
)
predict_sim_2layer <- pred_res_2layer$val
print_time("Predicting", pred_res_2layer$time)
usethis::use_data(predict_sim_2layer, overwrite = TRUE, compress = "xz")

# --- 1-Layer Fit (State Only) ---------------------------------------------
cat("\n=== 1-Layer Fit (State Only) ===\n")
locations_sim_1layer <- locations_sim[is.na(parent_id)]

populations_sim_1layer <- copy(populations_sim)
populations_sim_1layer[, loc_id := "State"]
populations_sim_1layer <- populations_sim_1layer[,
  .(weight = sum(weight)),
  by = .(obs_id, loc_id, cohort, age, dose)
]
observations_sim_1layer <- copy(observations_sim)

fit_res_1layer <- measure_time(sampling(
  observations_sim_1layer,
  populations_sim_1layer,
  locations_sim_1layer,
  stan_opts = st_opts
))
fit_sim_1layer <- fit_res_1layer$val
print_time("Fitting", fit_res_1layer$time)
print_stan_summary(fit_sim_1layer$raw_fit)

stopifnot(inherits(fit_sim_1layer$raw_fit, "stanfit"))
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

pred_res_1layer <- measure_time(
  predict(
    object = fit_sim_1layer,
    target = target_sim_1layer,
    posterior_size = 100
  )
)
predict_sim_1layer <- pred_res_1layer$val
print_time("Predicting", pred_res_1layer$time)
usethis::use_data(predict_sim_1layer, overwrite = TRUE, compress = "xz")
cat("\nFit and prediction artifacts updated successfully.\n")
