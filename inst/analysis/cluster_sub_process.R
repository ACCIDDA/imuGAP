library(data.table)
library(imuGAP)

.args <- if (interactive()) {
  .refpath <- file.path("~", "workspaces", "nc_measles")
  c(
    # inputs from data preparation
    file.path(
      .refpath,
      "output",
      "NC",
      c("cleaned_data.rds", "unit_key.rds", "vax_view.rds")
    ),
    file.path(.refpath, "catalytic_wts.csv"),
    # output
    file.path(.refpath, "model_results", "stanfit.rds")
  )
} else {
  commandArgs(trailingOnly = TRUE)
}

tarfile <- tail(.args, 1)
data_dt <- readRDS(.args[1]) |>
  as.data.table() |>
  subset(!(is.na(all_positive) | population == 0))

key_dt <- readRDS(.args[2]) |> as.data.table()
key_dt <- key_dt[type == "County", .(id = unit_id + 1L, parent_id = 1L)]
vv_dt <- readRDS(.args[3]) |> as.data.table()

reduce_unit_id <- data_dt[, .(unit_id = unique(unit_id) |> sort())]
reduce_unit_id[, replace_id := seq_len(.N) + key_dt[, max(id)]]

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
) |>
  unique()

# vax_view is the state, the outermost enclosing id
vv_location <- 1L
# vax_view child is first dose, otherwise 2nd dose
vv_dose <- function(obspop) fifelse(obspop == "child", 1L, 2L)

vv_wts <- vv_dt[
  wts_dt,
  on = .(pop, Age == months, Year == year),
  .(
    obs_id,
    location = vv_location,
    cohort,
    age = life_year,
    dose = vv_dose(pop),
    weight = wt
  ),
  nomatch = 0
]

data_wts <- data_dt[
  wts_dt[pop == "school"],
  on = .(year),
  .(
    obs_id,
    location = replace_id,
    cohort,
    age = life_year,
    dose = vv_dose(pop),
    weight = wt
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

res_stan <- imuGAP(
  observations,
  obs_population,
  locations,
  dose_schedule,
  stan_opts = stan_options(iter = 100, chains = 2)
)

# Stanify it

saveRDS(res_stan, tarfile)
saveRDS(rstan::extract(res_stan), gsub("(\\.[^\\.]+)$", "_extract\\1", tarfile))

library(ggplot2)

state_saturation <- (1 -
  plogis(rstan::extract(res_stan, pars = "logit_phi_st")$logit_phi_st)) |>
  as.data.table()
state_series <- state_saturation[, iteration := seq_len(.N)] |>
  melt.data.table(id.vars = "iteration", variable.name = "cohort")
state_series[, cohort := as.integer(gsub("V", "", cohort))]

d1_dt <- obs_population[location == 1 & dose == 1][
  observations,
  on = .(obs_id == id),
  nomatch = 0
]
d2_dt <- obs_population[location == 1 & dose == 2][
  observations,
  on = .(obs_id == id),
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
