
## Assumes imuGAP is in the same parent directory as nc_measles

library(tidyverse)
library(splines)
library(data.table)
library(EnvStats)

res_dt <- readRDS("../nc_measles/output/NC/cleaned_data.rds")

############### Simulate data for North Carolina ####################
n_yr <- 33
n_cohort <- 30
phi_st <- c(0.8401733, 0.8458791, 0.8515769, 0.8572586, 0.8629160, 0.8685411,
            0.8741259, 0.8796623, 0.8851422, 0.8905575, 0.8958959, 0.9011275,
            0.9062182, 0.9111339, 0.9158404, 0.9203035, 0.9244892, 0.9283632,
            0.9318916, 0.9350400, 0.9377351, 0.9397467, 0.9408054, 0.9407130,
            0.9395576, 0.9375024, 0.9347246, 0.9314054, 0.9277256, 0.9298663)
lambda1 <- 2.8
lambda2 <- 3.0

sigma_sch <- 0.8
sigma_cnty <- 0.4

# Dose schedule
doses <- matrix(0, nrow = n_yr, ncol  = 2)
doses[2:nrow(doses), 1] <- 1
doses[5:nrow(doses), 2] <- 1

cov <- matrix(nrow = n_yr, ncol = 2)
cov[1, ] <- 0

for (i in 2:n_yr) {
  cov[i, 1] <- cov[i - 1, 1] +
    (1 - cov[i - 1, 1]) * (1 - exp(-lambda1 * doses[i, 1]))
  cov[i, 2] <- cov[i - 1, 2] +
    ((cov[i, 1] - cov[i - 1, 2]) * (1 - exp(-lambda2 * doses[i, 2])))
}

# Pick 3 contiguous NC counties (Haywood, Jackson, Transylvania)
counties <- c(44, 50, 88)

sch_per_cnty <- res_dt |>
  filter(enc_unit_id %in% counties) |>
  group_by(enc_unit_id) |>
  mutate(enc_unit_id = cur_group_id()) |>
  ungroup() |>
  filter(year == 2024) |>
  group_by(enc_unit_id) |>
  summarize(n_sch = n())

cnty_offset <- rnorm(nrow(sch_per_cnty), 0, sigma_cnty)
sch_offset <- rnorm(sum(sch_per_cnty$n_sch), 0, sigma_sch)

# Simulate child vax view
n24 <- round(runif(n_cohort, 250, 450))
n36 <- round(runif(n_cohort, 250, 450))
sim_child <- bind_rows(data.frame(pop = "child",
                                  Year = 1:n_cohort,
                                  Age = "24 months",
                                  X = rbinom(n_cohort, n24, phi_st * cov[2, 1]),
                                  N = n24),
                       data.frame(pop = "child",
                                  Year = 1:n_cohort,
                                  Age = "36 months",
                                  X = rbinom(n_cohort, n36, phi_st * cov[3, 1]),
                                  N = n36))

# Simulate teen vax view
teen_yrs <- 18:30
sim_teen <- data.frame(pop = "teen",
                       Year = teen_yrs,
                       X = numeric(length(teen_yrs)),
                       N = numeric(length(teen_yrs)))

for (i in seq_len(nrow(sim_teen))) {
  samp_size <- round(runif(1, 40, 70))
  sim_teen$N[i] <- samp_size * 5
  sim_teen$X[i] <- sum(
    rbinom(
      5, samp_size,
      phi_st[(teen_yrs[i] - 17):(teen_yrs[i] - 13)] * cov[18:14, 2]
    )
  )
}

# Simulate school-level data
sch_yrs <- 6:30
nsch_base <- round(
  rlnormTrunc(sum(sch_per_cnty$n_sch), log(75), log(2.5), min = 10, max = 450)
)
kg_sim_full <- list()
ct <- 1
cnty_ids <- rep(sch_per_cnty$enc_unit_id, times = sch_per_cnty$n_sch)
for (s in seq_len(sum(sch_per_cnty$n_sch))) {
  nsch <- numeric(length(sch_yrs))
  nsch[1] <- nsch_base[s]
  for (y in 2:length(sch_yrs)) {
    nsch[y] <- round(runif(1, min = nsch[y - 1] - 5, max = nsch[y - 1] + 5))
    if (nsch[y] < 4) {
      nsch[y] <- 4
    }
  }
  offset <- sch_offset[s] + cnty_offset[cnty_ids[s]]
  cov_temp <- plogis(qlogis(phi_st[sch_yrs - 5]) + offset) * cov[5, 2]
  kg_sim_full[[s]] <- data.frame(
    year = sch_yrs,
    enc_unit_id = cnty_ids[s] + 1,
    unit_id = s,
    y_obs = rbinom(length(sch_yrs), nsch, cov_temp),
    y_smp = nsch
  )
}
kg_sim_full <- bind_rows(kg_sim_full)

# Randomly select 15% of kindergarten observations to treat as censored
cens <- sample(seq_len(nrow(kg_sim_full)),
               round(nrow(kg_sim_full) * 0.15),
               replace = FALSE)

kg_sim_full$censored <- NA
kg_sim_full$censored[cens] <- 1

# Simulate school vax view
annual_tots <- kg_sim_full |>
  group_by(year) |>
  summarize(tot_enr = sum(y_smp),
            tot_vax = sum(y_obs))

sim_school <- data.frame(
  pop = "school",
  Year = annual_tots$year,
  N = round(annual_tots$tot_enr * 0.9),
  X = rbinom(
    nrow(annual_tots), round(annual_tots$tot_enr * 0.9),
    phi_st[sch_yrs - 5] * cov[5, 2]
  )
)

# Bind vax view simulation together
vv_sim_full <- bind_rows(sim_child, sim_school, sim_teen)

# Subset data (for now full data)
vv_sim <- vv_sim_full
kg_sim <- kg_sim_full

# Canonicalize IDs
kg_sim <- kg_sim |>
  group_by(unit_id) |>
  mutate(unit_id = cur_group_id() + 4) |>
  ungroup()

# Assign county and school names
county_names <- c("Scruggs", "Simone", "Watson") # theme = musicians from NC
school_names <- c( # theme = native birds
  "Chickadee Elementary",
  "Nuthatch Academy",
  "Blue Heron School",
  "Flycatcher Elementary",
  "Bluebird Learning Center",
  "Catbird Academy",
  "Finch Elementary",
  "Sparrow School",
  "Towhee Children's Academy",
  "Warbler Elementary",
  "Egret Elementary",
  "Cardinal Academy",
  "Bunting School",
  "Tanager Academy",
  "Oriole Youth Academy",
  "Grosbeak Learning Center",
  "Junco Elementary",
  "Meadowlark School",
  "Goldfinch Elementary",
  "Mockingbird Academy",
  "Kinglet Learning Center",
  "Vireo School",
  "Kingfisher Academy",
  "Cormorant Elementary"
)

kg_sim$county <- county_names[kg_sim$enc_unit_id - 1]
kg_sim$school <- school_names[kg_sim$unit_id - 4]

kg_sim <- kg_sim |>
  select(loc_id = school, parent_id = county, year, enc_unit_id,
         unit_id, y_obs, y_smp, censored)

vv_sim <- vv_sim |>
  select(vaxview_type = pop, year = Year, age = Age, y_obs = X, y_smp = N) |>
  mutate(loc_id = "State")

# Put years in calendar terms
kg_sim$year <- kg_sim$year + 1995
vv_sim$year <- vv_sim$year + 1995

# Add in weight info
kg_sim$ly_min <- 5
kg_sim$ly_max <- 5
kg_sim$dose <- 2
kg_sim$weight <- 1

vv_sim$ly_min <- NA
vv_sim$ly_max <- NA
vv_sim$dose <- NA
vv_sim$weight <- NA
for (i in seq_len(nrow(vv_sim))) {
  if (vv_sim$vaxview_type[i] == "school") {
    vv_sim$ly_min[i] <- 5
    vv_sim$ly_max[i] <- 5
    vv_sim$dose[i] <- 2
    vv_sim$weight[i] <- 1
  } else if (vv_sim$vaxview_type[i] == "teen") {
    vv_sim$ly_min[i] <- 14
    vv_sim$ly_max[i] <- 18
    vv_sim$dose[i] <- 2
    vv_sim$weight[i] <- 1 / 5
  } else if (vv_sim$age[i] == "24 months") {
    vv_sim$ly_min[i] <- 2
    vv_sim$ly_max[i] <- 2
    vv_sim$dose[i] <- 1
    vv_sim$weight[i] <- 1
  } else {
    vv_sim$ly_min[i] <- 3
    vv_sim$ly_max[i] <- 3
    vv_sim$dose[i] <- 1
    vv_sim$weight[i] <- 1
  }
}

observations_sim <- bind_rows(kg_sim,
                              vv_sim |> mutate(unit_id = 1))

# Now get normalized cohorts
observations_sim <- observations_sim |>
  mutate(by_max = year - ly_min,
         by_min = year - ly_max,
         cohort_min = by_min - min(by_min) +  1,
         cohort_max = by_max - min(by_min) +  1) |>
  dplyr::select(-by_min, -by_max) |>
  dplyr::rename(positive = "y_obs", sample_n = "y_smp")

observations_sim$obs_id <- seq_len(nrow(observations_sim))

observations_sim <- as.data.table(observations_sim)

# Create populations
populations_sim <- data.frame(obs_id = numeric(),
                              loc_id = character(),
                              cohort = numeric(),
                              age = numeric(),
                              dose = numeric(),
                              weight = numeric())
for (i in seq_len(nrow(observations_sim))) {
  populations_sim <- bind_rows(
    populations_sim,
    data.frame(
      obs_id = observations_sim$obs_id[i],
      loc_id = observations_sim$loc_id[i],
      # remember cohort and ly are inverse
      cohort = observations_sim$cohort_max[i]:observations_sim$cohort_min[i],
      age = observations_sim$ly_min[i]:observations_sim$ly_max[i],
      dose = observations_sim$dose[i],
      weight = observations_sim$weight[i]
    )
  )
}

setDT(populations_sim)

# Create locations mapping
locations_sim <- bind_rows(
  # State
  data.frame(loc_id = "State",
             parent_id = NA),
  # Counties
  data.frame(loc_id = county_names,
             parent_id = "State"),
  #Schools
  unique(
    observations_sim |>
      filter(loc_id != "State") |>
      dplyr::select(loc_id,
                    parent_id)
  )
)

locations_sim <- as.data.table(locations_sim)

usethis::use_data(observations_sim, overwrite = TRUE)
usethis::use_data(populations_sim, overwrite = TRUE)
usethis::use_data(locations_sim, overwrite = TRUE)
