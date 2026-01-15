library(data.table)
library(imuGAP)

.args <- if (interactive()) {
  .refpath <- file.path("~", "workspaces", "nc_measles")
  c(
    # inputs from data preparation
    file.path(.refpath, "output", "NC",
      c("cleaned_data.rds", "unit_key.rds", "vax_view.rds")
    ),
    file.path(.refpath, "catalytic_wts.csv"),
    # output
    file.path(.refpath, "model_results", "stanfit.rds")
  )

} else commandArgs(trailingOnly = TRUE)

tarfile <- tail(.args, 1)
data_dt <- readRDS(.args[1]) |> as.data.table() |>
  subset(!(is.na(all_positive) | population == 0))

key_dt <- readRDS(.args[2]) |> as.data.table()
key_dt <- key_dt[type == "County", .(id = unit_id + 1L, parent_id = 1L)]
vv_dt <- readRDS(.args[3]) |> as.data.table()

reduce_unit_id <- data_dt[, .(unit_id = unique(unit_id) |> sort())]
reduce_unit_id[, replace_id := seq_len(.N) + key_dt[, max(id)] ]

data_dt[reduce_unit_id, on = .(unit_id), replace_id := replace_id]
data_dt[, obs_id := seq_len(.N)]

# package TODO: need a weighting function which translates from
#  - calendar/age time meta data [+ population; currently assumes uniform] =>
#  - weight on cohort/model_age time
# should be able to extract these from the observation meta-data, not have
# to read them in separately
wts_dt <- fread(.args[4])

vv_dt[, Age := as.integer(gsub(" Months", "", Age))]
vv_dt <- vv_dt[is.na(Age) | Age %in% c(24, 35)]
vv_dt[, obs_id := seq_len(.N) + data_dt[, max(obs_id)]]

# for pipeline: this should probably be the input file
locations <- rbind(
  key_dt,
  data_dt[, .(id = replace_id, parent_id = enc_unit_id + 1L)]
) |> unique()

# vax_view is the state, the outermost enclosing id
vv_location <- 1L
# vax_view child is first dose, otherwise 2nd dose
vv_dose <- function(obspop) fifelse(obspop == "child", 1L, 2L)

vv_wts <- vv_dt[
  wts_dt, on = .(pop, Age == months, Year == year), .(
    obs_id, location = vv_location, cohort, age = life_year,
    dose = vv_dose(pop), weight = wt
  ),
  nomatch = 0
]

data_wts <- data_dt[
  wts_dt[pop == "school"], on = .(year), .(
    obs_id, location = replace_id, cohort, age = life_year,
    dose = vv_dose(pop), weight = wt
  ),
  nomatch = 0
]

obs_population <- rbind(data_wts, vv_wts)
obs_population[, cohort := cohort - min(cohort) + 1L]

observations <- rbind(
  data_dt[, .(id = obs_id, positive = all_positive, sample_n = population)],
  vv_dt[, .(id = obs_id, positive = X, sample_n = N)]
)

dose_schedule <- c(1, 4)

res_stan <- imuGAP(observations, obs_population, locations, dose_schedule)

# Stanify it

saveRDS(res_stan, tarfile)
saveRDS(rstan::extract(res_stan), gsub("(\\.[^\\.]+)$", "_extract\\1", tarfile))

# plot(1-inv_logit(apply(rstan::extract(res_stan, pars = "logit_phi_st")$logit_phi_st, 2, mean)))

# Weights for imputation of current coverage snapshot
cohort_key <- wts_key %>%
  select(cohort, cohort_norm) %>%
  unique() %>% arrange(cohort) %>%
  mutate(ly = 2025 - cohort)

wts_pred <- data.frame(wts_school = rep(1:max(obs_sch$sch_id), each = nrow(cohort_key)),
                       wts_cohort = rep(cohort_key$cohort_norm, max(obs_sch$sch_id)),
                       wts_ly = 5,
                       wts_dose = rep(1, max(obs_sch$sch_id)),
                       wts = rep(1, max(obs_sch$sch_id)))

# For now do one and two dose coverage for everyone
wts_pred <- bind_rows(wts_pred,
                      wts_pred %>%
                        mutate(wts_dose = 2))


# Look at 2019 cohort when they were in kindergarten
# wts_pred <- bind_rows(wts_pred,
#                       wts_pred %>%
#                         filter(wts_cohort %in% c(30, 31)) %>%
#                         mutate(wts_ly = 5))

# Dose schedule
doses <- matrix(0, ncol  = 2, nrow = max(wts_pred$wts_ly))
doses[2:nrow(doses), 1] <- 1
doses[5:nrow(doses), 2] <- 1

dat_stan_pred <- list(n_yr = max(wts_pred$wts_ly),
                 n_cohort = max(wts$cohort),
                 n_sch = max(obs_sch$sch_id),
                 n_doses = 2,
                 dose_sched = doses,
                 k_bs = ncol(bsp),
                 bs = bsp,
                 n_obs = nrow(wts_pred),
                 y_obs = rep(1, nrow(wts_pred)), # don't get used for the non-inference version
                 y_smp = rep(1, nrow(wts_pred)), # don't get used for the non-inference version
                 n_weights = nrow(wts_pred),
                 obs_to_weights_bounds =1:nrow(wts_pred),
                 weights_school = wts_pred$wts_school,
                 weights_cohort = wts_pred$wts_cohort,
                 weights_life_year = wts_pred$wts_ly,
                 weights_dose = wts_pred$wts_dose,
                 weights = wts_pred$wts,
                 n_cnty = max(obs_sch$cnty_id),
                 cnty_bounds = unique(obs_sch$range_start),
                 predict_mode = 1)

preds <- gqs(sm, data = dat_stan_pred, as.matrix(res_stan))

saveRDS(preds, "model_results/process_v5_bsp_notrend_update_10.7_pred.rds")


# For now don't want this to run when submitting to the cluster
if(FALSE) {

  # # compare WAIC
  # models <- c("constantphi_trend",
  #             "bsp_trend",
  #             "bsp_notrend",
  #             "bsp7_notrend")
  # waic <- data.frame(waic_overall = numeric(length(models)),
  #                    waic_state = numeric(length(models)),
  #                    model = models)
  # ch <- list()
  # for(i in 1:length(models)) {
  #   ch[[i]] <- readRDS(paste0("model_results/process_", models[i], "_est.rds"))
  #   lik <- matrix(nrow = nrow(ch[[i]]$p_obs), ncol = ncol(ch[[i]]$p_obs))
  #   for(j in 1:nrow(ch[[i]]$p_obs)) {
  #     lik[j,] <- dbinom(dat_stan$y_obs, dat_stan$y_smp, ch[[i]]$p_obs[j,])
  #   }
  #   waic$waic_state[i] <- waic(lik[,17651:17698])$estimates[3,1]
  #   waic$waic_overall[i] <- waic(lik)$estimates[3,1]
  # }
  # waic$waic_overall_std <- waic$waic_overall - min(waic$waic_overall)
  # waic$waic_state_std <- waic$waic_state - min(waic$waic_state)
  #

  # Load estimates
  ch <- list()
  pred_est <- list()
  models <- c("v5_bsp_notrend_update_pred")
  for(i in 1:length(models)) {
    ch[[i]] <- extract(readRDS(paste0("model_results/process_", models[i], ".rds")))
    pred_est[[i]] <- data.frame(est = apply(ch[[i]]$p_gen, 2, mean),
                           ci_low = apply(ch[[i]]$p_gen, 2, quantile, 0.025),
                           ci_high = apply(ch[[i]]$p_gen, 2, quantile, 0.975),
                           cohort = 1989:2021,
                           model = models[i])
  }
  pred_est <- bind_rows(pred_est)

  # Get school sizes
  interpolate_enrollment <- function(dt) {
    setDT(dt) |> setkey(enc_unit_id, unit_id, year)
    dt[!is.na(population), if(.N != 1) {
      yrs <- min(year):max(year)
      apx <- approx(year, population, yrs)
      .(population = as.integer(apx$y), year = yrs)
    } else .SD[, .(population, year)], by = .(enc_unit_id, unit_id)] |>
      setkey(enc_unit_id, unit_id, year)
  }

  # Interpolate enrollment for missing years
  sch_enr <- res_dt %>%
    select(enc_unit_id, unit_id, population, year) %>%
    interpolate_enrollment() %>%
    mutate(cohort = year - 5)

  # Fill in enrollment outside school-data range
  fill_enr <- list()
  ct <- 1
  for(i in unique(sch_enr$unit_id)) {
    tmp <- sch_enr %>%
      filter(unit_id == i)
    if(min(tmp$cohort) == 2010) {
      fill_enr[[ct]] <- data.frame(enc_unit_id = tmp$enc_unit_id[1],
                                   unit_id = i,
                                   population = tmp$population[which.min(tmp$cohort)],
                                   year = (1989:2009) + 5,
                                   cohort = 1989:2009)
      ct <- ct + 1
    }
    if(max(tmp$cohort) == 2019) {
      fill_enr[[ct]] <- data.frame(enc_unit_id = tmp$enc_unit_id[1],
                                   unit_id = i,
                                   population = tmp$population[which.max(tmp$cohort)],
                                   year = (2020:2021) + 5,
                                   cohort = 2020:2021)
      ct <- ct + 1
    }
  }

  fill_enr <- bind_rows(fill_enr)
  sch_enr <- bind_rows(sch_enr,
                       fill_enr) %>%
    arrange(enc_unit_id, unit_id, cohort)

  pred_res <- pred_est %>%
    bind_cols(wts_pred %>%
                select(sch_id = wts_school,
                       cohort_norm = wts_cohort,
                       ly = wts_ly,
                       dose = wts_dose) %>%
                slice(rep(row_number(), length(ch)))) %>%
    left_join(cohort_key) %>%
    left_join(obs_sch %>%
                select(sch_id, unit_id) %>%
                unique()) %>%
    left_join(sch_enr %>%
                select(enc_unit_id, unit_id, cohort, population))

  # Look at total state-level coverage (sum of schools)
  st_cov <- pred_res %>%
    filter(!is.na(population)) %>%
    group_by(cohort, dose, model) %>%
    summarise(est = sum(est*population)/sum(population),
              ci_low = sum(ci_low*population)/sum(population),
              ci_high = sum(ci_high*population)/sum(population))

  # Look at teen VaxView observations
  vv_dat <- wts_key %>%
    left_join(vv_all %>%
                rename(year = Year) %>%
                filter(Age %in% c("24 Months", "35 Months") | pop != "child") %>%
                mutate(months = as.numeric(substr(Age, 1, 2)))) %>%
    mutate(vv_dose = ifelse(pop == "child", 1, 2)) %>%
    group_by(cohort, vv_dose) %>%
    summarize(vv_est = sum(wt*X/N)/sum(wt))

  ggplot() +
    geom_line(data = st_cov %>% filter(est > 0), aes(x = cohort, y = est, group = factor(dose), color = factor(dose))) +
    geom_ribbon(data = st_cov %>% filter(est > 0), aes(x = cohort, y = est, group = factor(dose), color = factor(dose), fill = factor(dose), ymin = ci_low, ymax = ci_high), alpha = 0.3) +
    geom_point(data = vv_dat, aes(y = vv_est, x = cohort, shape = factor(vv_dose)), alpha = 0.5) +
    labs(shape = "Dose", color = "Dose", fill = "Dose",
         y = "Coverage", x = "Birth cohort") +
    theme_bw() +
    theme(legend.position = "bottom")


  # Chains from original estimation run
  ch_est <- readRDS("model_results/process_v5_bsp_notrend_update_est.rds")
  phi_st <- inv_logit(data.frame(est = apply(ch_est$logit_phi_st[101:400,], 2, mean),
                       ci_low = apply(ch_est$logit_phi_st[101:400,], 2, quantile, 0.025),
                       ci_high = apply(ch_est$logit_phi_st[101:400,], 2, quantile, 0.975)))
  phi_st$cohort <- cohort_key$cohort

  ggplot(phi_st, aes(x = cohort, y = 1-est, group = 1)) +
    geom_line() +
    geom_ribbon(aes(ymin = 1-ci_low, ymax = 1-ci_high), alpha = 0.3)


  # Look at teen VaxView observations
  teen_check <- wts_key %>%
    filter(pop == "teen") %>%
    left_join(vv_teen %>%
                rename(year = Year)) %>%
    group_by(cohort) %>%
    summarize(est = sum(wt*X/N)/sum(wt))

  plot(teen_check$cohort, teen_check$est)
  plot(vv_teen$Year, vv_teen$X/vv_teen$N)


  # Look at distribution of offset and trend terms (mean)
  offsets <- bind_rows(
    data.frame(est = apply(ch_est$off_cnty, 2, mean),
               ci_low = apply(ch_est$off_cnty, 2, quantile, 0.025),
               ci_high = apply(ch_est$off_cnty, 2, quantile, 0.975),
               id = unique(obs_sch$enc_unit_id),
               param = "offset",
               type = "county"),
    data.frame(est = apply(ch_est$trend_cnty, 2, mean),
               ci_low = apply(ch_est$trend_cnty, 2, quantile, 0.025),
               ci_high = apply(ch_est$trend_cnty, 2, quantile, 0.975),
               id = unique(obs_sch$enc_unit_id),
               param = "trend",
               type = "county"),
    data.frame(est = apply(ch_est$off_sch, 2, mean),
               ci_low = apply(ch_est$off_sch, 2, quantile, 0.025),
               ci_high = apply(ch_est$off_sch, 2, quantile, 0.975),
               id = unique(obs_sch$unit_id),
               param = "offset",
               type = "school"),
    data.frame(est = apply(ch_est$trend_sch, 2, mean),
               ci_low = apply(ch_est$trend_sch, 2, quantile, 0.025),
               ci_high = apply(ch_est$trend_sch, 2, quantile, 0.975),
               id = unique(obs_sch$unit_id),
               param = "trend",
               type = "school"))

  off_med <- offsets %>%
    group_by(param, type) %>%
    summarize(median = median(est))

  ggplot(offsets) +
    geom_histogram(aes(est)) +
    geom_vline(data = off_med, aes(xintercept = median),
               color = "red", lty = "dashed") +
    facet_wrap(~param+type, scales = "free")

  # plot all offsets
  offsets_all <- bind_rows(data.frame(samp = as.vector(ch_est$off_cnty),
                                      type = "county",
                                      param = "offset"),
                           data.frame(samp = as.vector(ch_est$trend_cnty),
                                      type = "county",
                                      param = "trend"),
                           data.frame(samp = as.vector(ch_est$off_sch),
                                      type = "school",
                                      param = "offset"),
                           data.frame(samp = as.vector(ch_est$trend_sch),
                                      type = "school",
                                      param = "trend"))

  off_all_med <- offsets_all %>%
    group_by(param, type) %>%
    summarize(median = median(samp))

  ggplot(offsets_all) +
    geom_histogram(aes(samp)) +
    geom_vline(data = off_all_med, aes(xintercept = median),
               color = "red", lty = "dashed") +
    facet_wrap(~param+type, scales = "free")

  ggplot() +
    geom_line(data = st_cov %>% filter(dose == 2, cohort >= 2010),
              aes(x = cohort, y = est, group = model, color = model, fill = model)) +
    geom_ribbon(data = st_cov %>% filter(dose == 2, cohort >= 2010),
                aes(x = cohort, y = est, group = model, color = model, fill = model, ymin = ci_low, ymax = ci_high), alpha = 0.3) +
    geom_point(data = obs_sch %>% mutate(cohort = year - 5),
               aes(x = cohort, y = y_obs/y_smp)) +
    geom_smooth(data = obs_sch %>% mutate(cohort = year - 5),
                aes(x = cohort, y = y_obs/y_smp))

  sch_sum <- obs_sch %>%
    filter(y_smp > 0) %>%
    mutate(cohort = year - 5) %>%
    group_by(cohort) %>%
    summarize(size = sum(y_smp),
              sch_cov = sum((y_obs/y_smp)*y_smp)/sum(y_smp))

  ggplot() +
    geom_line(data = st_cov %>% filter(dose == 2, cohort >= 2010),
              aes(x = cohort, y = est, group = model, color = model, fill = model)) +
    geom_ribbon(data = st_cov %>% filter(dose == 2, cohort >= 2010),
                aes(x = cohort, y = est, group = model, color = model, fill = model, ymin = ci_low, ymax = ci_high), alpha = 0.3) +
    geom_point(data = sch_sum, aes(x = cohort, y = sch_cov)) +
    geom_smooth(data = sch_sum, aes(x = cohort, y = sch_cov))

  }

if(FALSE) {
  # Load estimates
  ch <- list()
  pred_est <- list()
  models <- c("v5_bsp_notrend_update_10.7")
  for(i in 1:length(models)) {
    ch[[i]] <- extract(readRDS(paste0("model_results/process_", models[i], "_pred.rds")))
    pred_est[[i]] <- data.frame(est = apply(ch[[i]]$p_gen, 2, mean),
                                ci_low = apply(ch[[i]]$p_gen, 2, quantile, 0.025),
                                ci_high = apply(ch[[i]]$p_gen, 2, quantile, 0.975),
                                cohort = 1989:2021,
                                model = models[i])
  }
  pred_est <- bind_rows(pred_est)

  # ch_est <- readRDS("model_results/process_v5_bsp_notrend_update_10.3_est.rds")
  # pred_est$lambda2 <- exp(mean(ch_est$lambda[,2]))

  # saveRDS(pred_est, "pred_est_intermediate_output_10.7.rds")
  # pred_est <- readRDS("pred_est_intermediate_output_10.7.rds")

  # Get school sizes
  interpolate_enrollment <- function(dt) {
    setDT(dt) |> setkey(enc_unit_id, unit_id, year)
    dt[!is.na(population), if(.N != 1) {
      yrs <- min(year):max(year)
      apx <- approx(year, population, yrs)
      .(population = as.integer(apx$y), year = yrs)
    } else .SD[, .(population, year)], by = .(enc_unit_id, unit_id)] |>
      setkey(enc_unit_id, unit_id, year)
  }

  # Interpolate enrollment for missing years
  sch_enr <- res_dt %>%
    select(enc_unit_id, unit_id, population, year) %>%
    interpolate_enrollment() %>%
    mutate(cohort = year - 5)

  # Fill in enrollment outside school-data range
  fill_enr <- list()
  ct <- 1
  for(i in unique(sch_enr$unit_id)) {
    tmp <- sch_enr %>%
      filter(unit_id == i)
    if(min(tmp$cohort) == 2010) {
      fill_enr[[ct]] <- data.frame(enc_unit_id = tmp$enc_unit_id[1],
                                   unit_id = i,
                                   population = tmp$population[which.min(tmp$cohort)],
                                   year = (1989:2009) + 5,
                                   cohort = 1989:2009)
      ct <- ct + 1
    }
    if(max(tmp$cohort) == 2019) {
      fill_enr[[ct]] <- data.frame(enc_unit_id = tmp$enc_unit_id[1],
                                   unit_id = i,
                                   population = tmp$population[which.max(tmp$cohort)],
                                   year = (2020:2021) + 5,
                                   cohort = 2020:2021)
      ct <- ct + 1
    }
  }

  fill_enr <- bind_rows(fill_enr)
  sch_enr <- bind_rows(sch_enr,
                       fill_enr) %>%
    arrange(enc_unit_id, unit_id, cohort)

  cohort_key$age_group <- paste(seq(from = 36, to = 4, by = -1),
                                seq(from = 37, to = 5, by = -1),
                                sep = "_")

  pred_res <- pred_est %>%
    bind_cols(wts_pred %>%
                select(sch_id = wts_school,
                       cohort_norm = wts_cohort,
                       ly = wts_ly,
                       dose = wts_dose)) %>%
    left_join(cohort_key %>%
                select(-ly)) %>%
    left_join(obs_sch %>%
                select(sch_id, unit_id) %>%
                unique()) %>%
    left_join(sch_enr %>%
                select(unit_id, cohort, population)) %>%
    left_join(sch_enr %>%
                select(enc_unit_id, unit_id) %>%
                unique())

  emp <- res_dt %>%
    filter(!is.na(all_positive),
           !is.na(population),
           population != 0) %>%
    mutate(cohort = year - 5) %>%
    select(-vax_positive, -exempt_vax_med, -exempt_vax_rel, -vax_catchup, -exempt_vax_tot) %>%
    mutate(pct_exempt = (exempt_any_rel + exempt_any_med)/population,
           pct_vax = all_positive/population,
           pct_catchup = any_catchup/population) %>%
    mutate(check = pct_vax + pct_catchup + pct_exempt)


  dash_out <- pred_res %>%
    left_join(emp %>%
                dplyr::select(unit_id, cohort, pct_vax, pct_exempt, check)) %>%
    # mutate(est2 = ifelse(check == 1 & !is.na(pct_exempt), pct_vax + (1-pct_vax-pct_exempt)*(1-exp(-lambda2*(ly-5))), est)) %>%
    mutate(est3 = ifelse(!is.na(pct_vax), pct_vax, est)) %>%
    filter(dose == 2) %>%
    rename(year = cohort_norm,
           cnty_id = enc_unit_id,
           id = unit_id) %>%
    mutate(est_orig = est) %>%
    mutate(est = ifelse(is.na(est3), est, est3),
           is_est = ifelse(is.na(est3), 1, 0))

  # dash_out <- pred_res %>%
  #   left_join(emp %>%
  #               dplyr::select(unit_id, cohort, pct_vax, pct_exempt, check)) %>%
  #   mutate(est2 = ifelse((check == 1 | cohort %in% c(2010, 2011)) & !is.na(pct_exempt), pct_vax + (1-pct_vax-pct_exempt)*(1-exp(-lambda2*(ly-5))), est)) %>%
  #   mutate(est3 = ifelse(!is.na(pct_vax), pct_vax, est)) %>%
  #   filter(dose == 2) %>%
  #   rename(year = cohort_norm,
  #          cnty_id = enc_unit_id,
  #          id = unit_id) %>%
  #   mutate(est = ifelse(is.na(est2), est, est2),
  #          ci_low = ifelse(is.na(est2), ci_low, NA),
  #          ci_high = ifelse(is.na(est2), ci_high, NA))

  saveRDS(dash_out, "model_results/dashboard_outputs_10.7.25.rds")

}



