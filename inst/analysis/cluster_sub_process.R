
library(data.table)
library(imuGAP)

.args <- if (interactive()) c(
  "stan_res.rds"
) else commandArgs(trailingOnly = TRUE)

# grab output file
tarfile <- tail(.args, 1)

# load the example data from the package
locs <- canonicalize_locations(locations_sim)
obs <- canonicalize_observations(observations_sim)
pop <- canonicalize_populations(obs_populations_sim, obs, locs)

res_stan <- imuGAP(
  obs, pop, locs,
  stan_opts = stan_options(iter = 100, chains = 2)
)

# store results
saveRDS(res_stan, tarfile)
saveRDS(rstan::extract(res_stan), gsub("(\\.[^\\.]+)$", "_extract\\1", tarfile))

library(ggplot2)

state_saturation <- (1 -
  plogis(rstan::extract(res_stan, pars = "logit_phi_st")$logit_phi_st)) |>
  as.data.table()
state_series <- state_saturation[, iteration := seq_len(.N)] |>
  melt.data.table(id.vars = "iteration", variable.name = "cohort")
state_series[, cohort := as.integer(gsub("V", "", cohort))]

d1_dt <- pop[loc_c_id == 1 & dose == 1][
  obs,
  on = .(obs_c_id == c_id),
  nomatch = 0
]
d2_dt <- pop[loc_c_id == 1 & dose == 2][
  obs,
  on = .(obs_c_id == c_id),
  nomatch = 0
]

ggplot(state_series, aes(x = cohort, y = value)) +
  geom_line(aes(group = iteration), alpha = 0.01) +
  geom_point(aes(y = positive / sample_n, shape = "d1"), data = d1_dt) +
  geom_point(aes(y = positive / sample_n, shape = "d2"), data = d2_dt) +
  stat_summary(fun = median, geom = "line", color = "red", size = 1.2) +
  labs(
    x = "Cohort",
    y = "State-level saturation"
  ) +
  theme_minimal()



