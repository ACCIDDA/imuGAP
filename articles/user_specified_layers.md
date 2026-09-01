# Flexible Location Layers in imuGAP

## Overview

The `imuGAP` package models vaccine coverage on nested population
partitions - e.g. a state divided into counties divided into schools. A
key strength of `imuGAP` is support for **flexible location layers**:
you can change the layer depth based on the location hierarchy you
provide. So, for example if you provide only the most aggregated
population (e.g. state-wide data), the model will have 1 layer, but if
you provide sub-populations (e.g. counties) there will be 2 layers. If
you also provide sub-sub-populations (e.g. schools), there will be 3
layers, and so on.

Whatever the data resolution, `imuGAP` canonicalizes the location tree,
maps parent-child relationships across layers, and estimates location
offset parameters for each resolution level (beyond fully aggregated
populations).

This vignette demonstrates:

1.  Exploring arbitrary location hierarchies and verifying layer depths.
2.  Constructing models for 1-layer (state-level only), 2-layer (state
    and county), and 3-layer (state, county, and school) datasets.
3.  Fitting and comparing parameter recovery and coverage trajectories
    across different layer resolutions.

------------------------------------------------------------------------

## The Example Location Hierarchy

Let’s first inspect the full hierarchy available in `locations_sim`:

| Scruggs County            | Simone County            | Watson County           |
|:--------------------------|:-------------------------|:------------------------|
| Chickadee Elementary      | Egret Elementary         | Meadowlark School       |
| Nuthatch Academy          | Cardinal Academy         | Goldfinch Elementary    |
| Blue Heron School         | Bunting School           | Mockingbird Academy     |
| Flycatcher Elementary     | Tanager Academy          | Kinglet Learning Center |
| Bluebird Learning Center  | Oriole Youth Academy     | Vireo School            |
| Catbird Academy           | Grosbeak Learning Center | Kingfisher Academy      |
| Finch Elementary          | Junco Elementary         | Cormorant Elementary    |
| Sparrow School            |                          |                         |
| Towhee Children’s Academy |                          |                         |
| Warbler Elementary        |                          |                         |

Most of our examples focus on using the higher resolution dataset
(i.e. including 3 layers, or down to schools). For this demonstration,
let’s create some alternative `location` data corresponding to different
data availability. Here, we’re using the convenient layer information
created by `canonicalize_locations`, but in real applications you would
of course only have the locations you have. These filtered views
simulate having high to low resolution data, correspond to all the
locations in the table (`locs_3layer`), the state and county locations
(`locs_2layer`), or just the state (`locs_1layer`).

``` r

locs_3layer <- canonicalize_locations(locations_sim) # all the data
locs_2layer <- locs_3layer[layer <= 2] # state and county only
locs_1layer <- locs_3layer[layer <= 1] # state only
```

Similarly, we filter our observations and associated metadata
(i.e. populations) to match each location resolution:

``` r

data("observations_sim", package = "imuGAP")
data("populations_sim", package = "imuGAP")
data("latent_params_sim", package = "imuGAP")

# 3-Layer views (State, County, School)
# 1. Filter populations to locations present in locs_3layer
pops_3layer <- populations_sim[loc_id %in% locs_3layer$loc_id]
# 2. Extract relevant observations corresponding to filtered populations
obs_3layer <- observations_sim[obs_id %in% pops_3layer$obs_id]
# 3. Confirm extracted observations don't have unpresent locations in original population data
stopifnot(!populations_sim[obs_id %in% obs_3layer$obs_id, any(!loc_id %in% locs_3layer$loc_id)])

# 2-Layer views (State and County)
# 1. Filter populations to locations present in locs_2layer
pops_2layer <- populations_sim[loc_id %in% locs_2layer$loc_id]
# 2. Extract relevant observations corresponding to filtered populations
obs_2layer <- observations_sim[obs_id %in% pops_2layer$obs_id]
# 3. Confirm extracted observations don't have unpresent locations in original population data
stopifnot(!populations_sim[obs_id %in% obs_2layer$obs_id, any(!loc_id %in% locs_2layer$loc_id)])

# 1-Layer views (State only)
# 1. Aggregate and filter populations to State level (locs_1layer)
pops_1layer <- copy(populations_sim)[, loc_id := "State"]
pops_1layer <- pops_1layer[, .(weight = sum(weight)), by = .(obs_id, loc_id, cohort, age, dose)]
pops_1layer <- pops_1layer[loc_id %in% locs_1layer$loc_id]
# 2. Extract relevant observations corresponding to filtered populations
obs_1layer <- observations_sim[obs_id %in% pops_1layer$obs_id]
# 3. Confirm extracted observations don't have unpresent locations in population data
stopifnot(!pops_1layer[obs_id %in% obs_1layer$obs_id, any(!loc_id %in% locs_1layer$loc_id)])
```

------------------------------------------------------------------------

## 2. Fitting the Model

To fit the model at different resolutions looks basically the same: just
use 1-layer, 2-layer, or 3-layer location inputs. Note that no other
setting tweaks are required, but these can take some time to run; if
following along, you probably want to load the results from the package
data.

``` r

st_opts <- stan_options(iter = 1000, chains = 4, seed = 1L)

# various layer fits
fit_3layer <- sampling(obs_3layer, pops_3layer, locs_3layer, stan_opts = st_opts)
fit_2layer <- sampling(obs_2layer, pops_2layer, locs_2layer, stan_opts = st_opts)
fit_1layer <- sampling(obs_1layer, pops_1layer, locs_1layer, stan_opts = st_opts)
```

To load precomputed results, get the following items from package data:

``` r

data("fit_sim", package = "imuGAP")
data("fit_sim_2layer", package = "imuGAP")
data("fit_sim_1layer", package = "imuGAP")
```

## 3. Using the Fits to Predict Coverage

Predicting coverage from fitted models involves providing a target grid
for the desired locations, ages, cohorts, and doses, and then calling
[`predict()`](https://rdrr.io/r/stats/predict.html). We can load the
example target dataset bundled with `imuGAP` package data and filter it
to match each location resolution:

``` r

data("target_sim", package = "imuGAP")

# 3-Layer Prediction (State, County, School)
target_3layer <- target_sim[loc_id %in% locs_3layer$loc_id]
predict_3layer <- predict(object = fit_3layer, target = target_3layer, posterior_size = 100)

# 2-Layer Prediction (State and County)
target_2layer <- target_sim[loc_id %in% locs_2layer$loc_id]
predict_2layer <- predict(object = fit_2layer, target = target_2layer, posterior_size = 100)

# 1-Layer Prediction (State only)
target_1layer <- target_sim[loc_id %in% locs_1layer$loc_id]
predict_1layer <- predict(object = fit_1layer, target = target_1layer, posterior_size = 100)
```

If you are working along through this vignette, you may wish to simply
load the pre-computed prediction results, as we bundled them with
`imuGAP` package data:

``` r

data("predict_sim", package = "imuGAP")
data("predict_sim_2layer", package = "imuGAP")
data("predict_sim_1layer", package = "imuGAP")
```

You can summarize the predictions using
[`summary()`](https://rdrr.io/r/base/summary.html) to obtain posterior
median (`q50`) and 95% credible intervals (`q2_5`, `q97_5`):

``` r

summary_3layer <- summary(predict_sim)
summary_2layer <- summary(predict_sim_2layer)
summary_1layer <- summary(predict_sim_1layer)
```

------------------------------------------------------------------------

## 4. Comparing Different Resolution Fits

As seen in previous sections, the actual fitting and prediction steps
are identical irrespective of data resolution. Let’s see how the
different resolutions affect parameter and coverage estimation.

First, compare estimated State-level coverage from the 1-layer, 2-layer,
and 3-layer model fits against the latent coverage stored in
`latent_params_sim$coverage`. Faceting by data resolution (columns)
shows that macro State trends are estimated accurately regardless of
whether lower-level sub-population data are provided:

**Show plot code**

``` r

state_name <- locations_sim[is.na(parent_id), loc_id]

state_1layer_pred <- summary_1layer[
  loc_id == state_name & dose == 2 & age > 4
][, hierarchy := "1-Layer (State Only)"]
state_2layer_pred <- summary_2layer[
  loc_id == state_name & dose == 2 & age > 4
][, hierarchy := "2-Layer (State/County)"]
state_3layer_pred <- summary_3layer[
  loc_id == state_name & dose == 2 & age > 4
][, hierarchy := "3-Layer (State/County/School)"]

state_cov_pred <- rbindlist(list(
  state_1layer_pred,
  state_2layer_pred,
  state_3layer_pred
))
h_levels_3 <- c(
  "1-Layer (State Only)",
  "2-Layer (State/County)",
  "3-Layer (State/County/School)"
)
state_cov_pred[, hierarchy := factor(hierarchy, levels = h_levels_3)]

state_idx <- predict_sim$target[
  loc_id == state_name & dose == 2 & age > 4,
  which = TRUE
]
true_state_df <- data.table(
  age = predict_sim$target[state_idx, age],
  coverage = latent_params_sim$coverage[state_idx]
)

true_state_faceted <- rbindlist(lapply(h_levels_3, function(h) {
  df <- copy(true_state_df)
  df[, hierarchy := factor(h, levels = h_levels_3)]
  df
}))

ggplot() +
  geom_ribbon(
    data = state_cov_pred,
    aes(x = age, ymin = q2_5, ymax = q97_5, fill = "Estimated (95% CI)"),
    alpha = 0.2
  ) +
  geom_line(
    data = state_cov_pred,
    aes(x = age, y = q50, color = "Estimated (Median)"),
    linewidth = 1
  ) +
  geom_point(
    data = true_state_faceted,
    aes(x = age, y = coverage, shape = "True Coverage"),
    color = "black",
    size = 2.2
  ) +
  facet_grid(. ~ hierarchy) +
  scale_shape_manual(name = "", values = c("True Coverage" = 17)) +
  scale_color_manual(name = "", values = c("Estimated (Median)" = "#1b9e77")) +
  scale_fill_manual(name = "", values = c("Estimated (95% CI)" = "#1b9e77")) +
  theme_bw() +
  scale_x_continuous(breaks = seq(5, 18, by = 3), minor_breaks = NULL) +
  scale_y_continuous(limits = c(0.8, 1.0)) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.02, 0.05),
    legend.justification.inside = c(0, 0),
    legend.direction = "horizontal",
    legend.box = "horizontal"
  ) +
  labs(
    x = "Age",
    y = "State-Level Two-Dose Coverage"
  )
```

![State-level coverage comparison across 1-layer, 2-layer, and 3-layer
model fits against true
values.](user_specified_layers_files/figure-html/state-plot-1.png)

State-level coverage comparison across 1-layer, 2-layer, and 3-layer
model fits against true values.

### County-Level Coverage Across Data Resolutions

Next, we compare County-level coverage estimates between the 2-layer and
3-layer model fits across counties (*Scruggs*, *Simone*, *Watson*):

**Show plot code**

``` r

counties <- locations_sim[parent_id == state_name, loc_id]

county_2layer_pred <- summary_2layer[
  loc_id %in% counties & dose == 2 & age > 4
][, hierarchy := "2-Layer (State/County)"]
county_3layer_pred <- summary_3layer[
  loc_id %in% counties & dose == 2 & age > 4
][, hierarchy := "3-Layer (State/County/School)"]

county_cov_pred <- rbindlist(list(county_2layer_pred, county_3layer_pred))
h_levels_county <- c("2-Layer (State/County)", "3-Layer (State/County/School)")
county_cov_pred[, hierarchy := factor(hierarchy, levels = h_levels_county)]
county_cov_pred[, loc_id := factor(
  loc_id,
  levels = c("Simone", "Watson", "Scruggs")
)]

county_idx <- predict_sim$target[
  loc_id %in% counties & dose == 2 & age > 4,
  which = TRUE
]
true_county_df <- data.table(
  loc_id = factor(
    predict_sim$target[county_idx, loc_id],
    levels = c("Simone", "Watson", "Scruggs")
  ),
  age = predict_sim$target[county_idx, age],
  coverage = latent_params_sim$coverage[county_idx]
)

true_county_faceted <- rbindlist(lapply(h_levels_county, function(h) {
  df <- copy(true_county_df)
  df[, hierarchy := factor(h, levels = h_levels_county)]
  df
}))

ggplot() +
  geom_ribbon(
    data = county_cov_pred,
    aes(x = age, ymin = q2_5, ymax = q97_5, fill = loc_id),
    alpha = 0.15
  ) +
  geom_line(
    data = county_cov_pred,
    aes(x = age, y = q50, color = loc_id),
    linewidth = 0.9
  ) +
  geom_point(
    data = true_county_faceted,
    aes(x = age, y = coverage, color = loc_id, shape = "True Coverage"),
    size = 2
  ) +
  facet_grid(. ~ hierarchy) +
  scale_shape_manual(name = "", values = c("True Coverage" = 18)) +
  scale_color_discrete(NULL, aesthetics = c("color", "fill")) +
  scale_x_continuous(breaks = seq(5, 18, by = 3), minor_breaks = NULL) +
  scale_y_continuous(limits = c(0.8, 1.0)) +
  theme_bw() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.02, 0.05),
    legend.justification.inside = c(0, 0),
    legend.direction = "horizontal",
    legend.box = "horizontal"
  ) +
  labs(
    x = "Age",
    y = "County-Level Two-Dose Coverage"
  )
```

![County-level coverage comparison: 2-layer vs. 3-layer model estimates
against true
values.](user_specified_layers_files/figure-html/county-plot-1.png)

County-level coverage comparison: 2-layer vs. 3-layer model estimates
against true values.

### Force of Vaccination ($`\lambda`$) Estimation

We also examine the model’s ability to estimate the underlying force of
vaccination parameters ($`\lambda`$) across different location data
resolutions.

**Show plot code**

``` r

extract_lambda_summary <- function(fit, label) {
  draws <- rstan::extract(fit$stanfit, pars = "lambda_raw")$lambda_raw
  doses_factor <- factor(c(
    rep("Dose 1", nrow(draws)),
    rep("Dose 2", nrow(draws))
  ))
  data.table(
    hierarchy = label,
    dose = doses_factor,
    lambda_raw = c(draws[, 1], draws[, 2])
  )[, .(
    q50 = stats::median(lambda_raw),
    q2_5 = stats::quantile(lambda_raw, 0.025),
    q97_5 = stats::quantile(lambda_raw, 0.975)
  ), by = .(hierarchy, dose)]
}

lambda_est <- rbindlist(list(
  extract_lambda_summary(fit_sim_1layer, "1-Layer"),
  extract_lambda_summary(fit_sim_2layer, "2-Layer"),
  extract_lambda_summary(fit_sim, "3-Layer")
))
h_levels_lambda <- c("1-Layer", "2-Layer", "3-Layer")
lambda_est[, hierarchy := factor(hierarchy, levels = h_levels_lambda)]

true_lambda <- data.table(
  dose = factor(c("Dose 1", "Dose 2")),
  hierarchy = factor("1-Layer", levels = h_levels_lambda),
  true_val = log(latent_params_sim$lambda),
  label = sprintf("True~lambda == %.1f", latent_params_sim$lambda)
)

ggplot(lambda_est, aes(x = hierarchy, y = q50)) +
  geom_hline(
    data = true_lambda,
    aes(yintercept = true_val),
    color = "firebrick",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  geom_label(
    data = true_lambda,
    aes(x = hierarchy, y = true_val, label = label),
    parse = TRUE,
    color = "firebrick",
    fill = ggplot2::alpha("white", 0.75),
    linewidth = NA,
    vjust = -0.3,
    hjust = 0.1,
    size = 3.2
  ) +
  geom_pointrange(
    aes(ymin = q2_5, ymax = q97_5),
    size = 0.7
  ) +
  facet_wrap(~dose) +
  coord_cartesian(ylim = c(0.5, 1.5)) +
  scale_y_continuous(
    transform = "exp",
    labels = function(x) sprintf("%.2f", exp(x))
  ) +
  theme_bw() +
  labs(
    x = "Data Resolution (Model Hierarchy)",
    y = "Uptake Rate (exponential scale)"
  )
```

![Force of vaccination (lambda) estimates across data resolutions
compared to true
values.](user_specified_layers_files/figure-html/lambda-plot-1.png)

Force of vaccination (lambda) estimates across data resolutions compared
to true values.

------------------------------------------------------------------------

## Conclusion

Support for **flexible location layers** allows `imuGAP` to adapt to
whatever location hierarchy you provide. - If you provide only
state-level aggregated data (**1 layer**), macro trends are estimated
accurately. - If you provide sub-populations like counties (**2
layers**) or sub-sub-populations like schools (**3 layers**), the model
automatically builds hierarchical random offsets down the location tree
to capture finer resolution variation.
