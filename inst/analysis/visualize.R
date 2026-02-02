library(tidyverse)
library(rstan)

source("../nc_measles/code/Utils.R")

# Plot overall state coverage vs. aggregated school data
res <- readRDS("../nc_measles/model_results/dashboard_outputs_10.7.25.rds") %>% select(-est3, -is_est)

st_est <- res %>%
  filter(!is.na(population)) %>%
  group_by(cohort) %>%
  summarize(est = sum(est_orig*population)/sum(population),
            ci_low = sum(ci_low*population)/sum(population),
            ci_high = sum(ci_high*population)/sum(population))

st_obs <-  data_dt %>%
  mutate(cohort = year - 5) %>%
  group_by(cohort) %>%
  summarize(up_to_date = sum(all_positive, na.rm = T)/sum(population[!is.na(all_positive)]),
            non_exempt = sum(population - exempt_any_tot, na.rm = T)/sum(population[!is.na(exempt_any_tot)])) %>%
  pivot_longer(-cohort)

ggplot() +
  geom_line(data = st_est %>% filter(cohort != 2021), aes(x = cohort, y = est)) +
  geom_ribbon(data = st_est %>% filter(cohort != 2021), aes(x = cohort, ymin = ci_low, ymax = ci_high),
              alpha = 0.5, fill = "gray") +
  geom_point(data = st_obs, aes(x = cohort, y = value, color = name)) +
  theme_bw() +
  labs(y = "Coverage", x = "Cohort", color = "")


# Look at actual vs. estiamted nnumber of students vaccinated
res %>%
  filter(!is.na(pct_vax)) %>%
  mutate(est_vax = est_orig*population,
         act_vax = est*population) %>%
  ggplot(aes(x = est_vax, y = act_vax)) +
  geom_point() +
  geom_abline(a = 0, b = 1, color = "blue") +
  theme_bw() +
  labs(x = "Estimates", y = "Actual")
