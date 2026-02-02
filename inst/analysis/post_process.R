
  # Load estimates
  ch <- list()
  pred_est <- list()
  models <- c("v5_bsp_notrend_update_10.7")
  for(i in 1:length(models)) {
    ch[[i]] <- rstan::extract(readRDS(paste0("../nc_measles/model_results/process_", models[i], "_pred.rds")))
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
  
  cohort_key$age_group <- paste(seq(from = 36, to = 4, by = -1),
                                seq(from = 37, to = 5, by = -1),
                                sep = "_")
  
  # Join school size and ids
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
  
  # Join empirical data
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
  
  # Replace estimates with empirical data where available
  dash_out <- pred_res %>%
    left_join(emp %>%
                dplyr::select(unit_id, cohort, pct_vax, pct_exempt, check)) %>%
    filter(dose == 2) %>%
    rename(year = cohort_norm,
           cnty_id = enc_unit_id,
           id = unit_id) %>%
    mutate(est_orig = est) %>%
    mutate(est = ifelse(!is.na(pct_vax), pct_vax, est))
  
  # Save results
  saveRDS(dash_out, "model_results/dashboard_outputs_10.7.25.rds")