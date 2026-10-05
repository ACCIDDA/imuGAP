# Getting Started with imuGAP

## Introduction

`imuGAP` (Immunity: Geographic & Age-based Projection) estimates an
underlying process model for vaccination on an arbitrary hierarchical
population structure, which can then be used to predict vaccination
coverage. Internally, `imuGAP` performs Bayesian inference via the Stan
probabilistic programming language.

The package enables researchers and public health analysts to synthesize
across multiple heterogeneous surveillance streams (such as national
surveys, state school entry records, and local coverage censuses) to:

1.  **Estimate current vaccination coverage** across arbitrary
    sub-populations and geographic levels.
2.  **Project cohort trajectories** (e.g. coverage at each age from
    birth through adolescence for a given birth cohort).
3.  **Impute missing data gaps** across unobserved years, age groups, or
    sub-regions by leveraging inferred relationships via the process
    model.

#### Core Model Intuition

The core `imuGAP` model represents each target population slice
$`(i, a)`$ (location $`i`$, cohort born at time $`a`$) as having an
underlying non-uptake probability $`\phi_{i,a}`$, with complementary
uptake probability $`1 - \phi_{i,a}`$. Given a dose eligibility schedule
$`\nu(\tau)`$ (in terms of time-in-life) and a force-of-vaccination rate
$`\lambda(t)`$ (in terms of absolute time), cumulative coverage at age
$`t`$ for the first dose is modeled as:

``` math
\Pr_{i, a}(\ge\textrm{1 dose}\mid t) = \left(1 - \phi_{i, a}\right) \left(1 - \exp\left\{-\int_a^{t} \lambda(s)\nu(s-a) \mathrm{d}s\right\}\right)
```

This formulation extends sequentially to multiple vaccine doses,
conditional on receipt of prior doses. Sequential multi-dose regimens
(e.g. Dose 1 at $`\ge 12`$ months, Dose 2 at $`\ge 4`$ years) are
modeled as a continuous-time Markov transition process.

------------------------------------------------------------------------

## The Fit & Predict Loop

The following diagram shows the end-to-end fit and predict workflow:

![](figures/fit_predict_workflow.svg)

------------------------------------------------------------------------

### 1. Preparing the Inputs

`imuGAP` requires three core input tables:

1.  `locations`: The nested population hierarchy (e.g. State
    $`\rightarrow`$ County $`\rightarrow`$ School). This may optionally
    include population sizes, which may be necessary to ensure accurate
    results when entities within the same layer differ substantially in
    size.
2.  `observations`: Empirical survey counts (`positive` and `sample_n`)
    and censoring indicators.
3.  `populations`: Metadata and fractional weights mapping observations
    to discrete location/age/cohort/dose slices.

``` r

library(imuGAP)
library(data.table)
library(ggplot2)

data("locations_sim", package = "imuGAP")
data("observations_sim", package = "imuGAP")
data("populations_sim", package = "imuGAP")

head(locations_sim)
#>                  loc_id population parent_id
#>                  <char>      <num>    <char>
#> 1:                State  2895.1333      <NA>
#> 2:              Scruggs  1527.7000     State
#> 3:               Simone   746.6333     State
#> 4:               Watson   620.8000     State
#> 5: Chickadee Elementary   147.8333   Scruggs
#> 6:     Nuthatch Academy   368.5333   Scruggs
head(observations_sim[, .(obs_id, loc_id, positive, sample_n, censored)])
#>    obs_id               loc_id positive sample_n censored
#>     <int>               <char>    <num>    <int>    <num>
#> 1:      1 Chickadee Elementary      135      155       NA
#> 2:      2 Chickadee Elementary      124      152       NA
#> 3:      3 Chickadee Elementary      133      156       NA
#> 4:      4 Chickadee Elementary      127      155       NA
#> 5:      5 Chickadee Elementary      141      155       NA
#> 6:      6 Chickadee Elementary      139      158       NA
head(populations_sim)
#>    obs_id               loc_id cohort   age  dose weight
#>     <int>               <char>  <int> <int> <int>  <num>
#> 1:      1 Chickadee Elementary      1     5     2      1
#> 2:      2 Chickadee Elementary      2     5     2      1
#> 3:      3 Chickadee Elementary      3     5     2      1
#> 4:      4 Chickadee Elementary      4     5     2      1
#> 5:      5 Chickadee Elementary      5     5     2      1
#> 6:      6 Chickadee Elementary      6     5     2      1
```

Input datasets are automatically validated and canonicalized during
sampling via
[`canonicalize_locations()`](https://accidda.github.io/imuGAP/reference/canonicalize.md),
[`canonicalize_observations()`](https://accidda.github.io/imuGAP/reference/canonicalize.md),
and
[`canonicalize_populations()`](https://accidda.github.io/imuGAP/reference/canonicalize.md).

For an in-depth walkthrough of flexible surveillance stream
structures—including right-censored observations (such as when
surveillance reports only complete coverage for a bundle of vaccines,
not specifically the target vaccine) and composite observations that mix
multiple age groups or populations—see **[Included Example Datasets and
Data
Validation](https://accidda.github.io/imuGAP/articles/example_data.md)**.

------------------------------------------------------------------------

### 2. Fitting the Model

You can fit the model to your data by calling
[`sampling()`](https://accidda.github.io/imuGAP/reference/sampling.md).
Configure model options (such as B-spline degrees of freedom and dose
eligibility ages) with
[`imugap_options()`](https://accidda.github.io/imuGAP/reference/imugap_options.md),
and specify MCMC sampler settings (chains, iterations, backend) with
[`stan_options()`](https://accidda.github.io/flexstanr/reference/stan_options.html):

``` r

# Configure model options
im_opts <- imugap_options(df = 5L, dose_schedule = c(1L, 4L))

# Configure Stan MCMC sampler
st_opts <- stan_options(chains = 4, iter = 2000, seed = 1L)

# Fit the model
fit <- sampling(
  observations = observations_sim,
  populations = populations_sim,
  locations = locations_sim,
  imugap_opts = im_opts,
  stan_opts = st_opts
)
```

For this vignette, you can load the pre-computed fit fixture `fit_sim`
bundled with the package:

``` r

data("fit_sim", package = "imuGAP")
fit_sim
#> An imuGAP model fit (`imugap_fit`):
#>   Hierarchy:    28 locations across 3 layers (root: 'State')
#>   Observations: 841 total (775 uncensored, 66 right-censored)
#> 
#> Inference for Stan model: impute_school_coverage_process_v6.
#> 4 chains, each with iter=1000; warmup=500; thin=1; 
#> post-warmup draws per chain=500, total post-warmup draws=2000.
#> 
#>                       mean se_mean   sd      2.5%       25%       50%       75%
#> beta_bs[1]           -1.68    0.00 0.03     -1.75     -1.70     -1.68     -1.66
#> beta_bs[2]           -1.88    0.00 0.05     -1.98     -1.92     -1.88     -1.85
#> beta_bs[3]           -2.48    0.00 0.08     -2.64     -2.53     -2.48     -2.42
#> beta_bs[4]           -3.13    0.00 0.09     -3.31     -3.19     -3.13     -3.07
#> beta_bs[5]           -2.59    0.00 0.09     -2.77     -2.65     -2.59     -2.53
#> sigma_layer[1]        0.56    0.01 0.32      0.19      0.33      0.48      0.70
#> sigma_layer[2]        0.75    0.01 0.13      0.55      0.65      0.73      0.82
#> lambda_raw[1]         1.02    0.00 0.03      0.97      1.00      1.02      1.04
#> lambda_raw[2]         1.06    0.00 0.01      1.03      1.05      1.06      1.07
#> raw_phi_root[1]      -1.68    0.00 0.03     -1.75     -1.70     -1.68     -1.66
#> raw_phi_root[2]      -1.72    0.00 0.03     -1.77     -1.74     -1.72     -1.70
#> raw_phi_root[3]      -1.77    0.00 0.02     -1.81     -1.78     -1.77     -1.75
#> raw_phi_root[4]      -1.82    0.00 0.02     -1.86     -1.83     -1.82     -1.80
#> raw_phi_root[5]      -1.87    0.00 0.02     -1.91     -1.88     -1.87     -1.85
#> raw_phi_root[6]      -1.92    0.00 0.03     -1.97     -1.94     -1.92     -1.90
#> raw_phi_root[7]      -1.97    0.00 0.03     -2.02     -1.99     -1.97     -1.96
#> raw_phi_root[8]      -2.03    0.00 0.03     -2.08     -2.05     -2.03     -2.01
#> raw_phi_root[9]      -2.09    0.00 0.03     -2.14     -2.11     -2.09     -2.07
#> raw_phi_root[10]     -2.15    0.00 0.03     -2.20     -2.17     -2.15     -2.13
#> raw_phi_root[11]     -2.21    0.00 0.03     -2.26     -2.23     -2.21     -2.19
#> raw_phi_root[12]     -2.27    0.00 0.03     -2.33     -2.29     -2.27     -2.25
#> raw_phi_root[13]     -2.33    0.00 0.03     -2.39     -2.35     -2.33     -2.31
#> raw_phi_root[14]     -2.40    0.00 0.03     -2.46     -2.42     -2.39     -2.37
#> raw_phi_root[15]     -2.46    0.00 0.03     -2.53     -2.48     -2.46     -2.44
#> raw_phi_root[16]     -2.52    0.00 0.04     -2.60     -2.55     -2.52     -2.50
#> raw_phi_root[17]     -2.59    0.00 0.04     -2.66     -2.61     -2.59     -2.56
#> raw_phi_root[18]     -2.65    0.00 0.04     -2.73     -2.68     -2.65     -2.62
#> raw_phi_root[19]     -2.71    0.00 0.04     -2.79     -2.73     -2.71     -2.68
#> raw_phi_root[20]     -2.76    0.00 0.04     -2.85     -2.79     -2.76     -2.73
#> raw_phi_root[21]     -2.80    0.00 0.04     -2.90     -2.83     -2.80     -2.77
#> raw_phi_root[22]     -2.84    0.00 0.05     -2.94     -2.87     -2.84     -2.81
#> raw_phi_root[23]     -2.87    0.00 0.05     -2.96     -2.90     -2.87     -2.83
#> raw_phi_root[24]     -2.88    0.00 0.05     -2.97     -2.91     -2.88     -2.84
#> raw_phi_root[25]     -2.88    0.00 0.05     -2.97     -2.91     -2.87     -2.84
#> raw_phi_root[26]     -2.86    0.00 0.05     -2.95     -2.89     -2.86     -2.82
#> raw_phi_root[27]     -2.82    0.00 0.05     -2.92     -2.85     -2.82     -2.79
#> raw_phi_root[28]     -2.77    0.00 0.06     -2.88     -2.80     -2.76     -2.73
#> raw_phi_root[29]     -2.69    0.00 0.07     -2.83     -2.73     -2.69     -2.64
#> raw_phi_root[30]     -2.59    0.00 0.09     -2.77     -2.65     -2.59     -2.53
#> lp__             -78865.07    0.37 5.25 -78876.30 -78868.29 -78864.57 -78861.59
#>                      97.5% n_eff Rhat
#> beta_bs[1]           -1.61  1127 1.00
#> beta_bs[2]           -1.78   987 1.00
#> beta_bs[3]           -2.32   845 1.00
#> beta_bs[4]           -2.95   931 1.00
#> beta_bs[5]           -2.41  1125 1.00
#> sigma_layer[1]        1.36   543 1.01
#> sigma_layer[2]        1.07   193 1.02
#> lambda_raw[1]         1.07  1268 1.00
#> lambda_raw[2]         1.09  1113 1.00
#> raw_phi_root[1]      -1.61  1127 1.00
#> raw_phi_root[2]      -1.67  1157 1.00
#> raw_phi_root[3]      -1.72  1140 1.00
#> raw_phi_root[4]      -1.77  1118 1.00
#> raw_phi_root[5]      -1.82  1127 1.00
#> raw_phi_root[6]      -1.87  1165 1.00
#> raw_phi_root[7]      -1.92  1219 1.00
#> raw_phi_root[8]      -1.98  1276 1.00
#> raw_phi_root[9]      -2.04  1306 1.00
#> raw_phi_root[10]     -2.10  1300 1.00
#> raw_phi_root[11]     -2.16  1259 1.00
#> raw_phi_root[12]     -2.21  1206 1.00
#> raw_phi_root[13]     -2.27  1160 1.00
#> raw_phi_root[14]     -2.33  1133 1.00
#> raw_phi_root[15]     -2.39  1126 1.00
#> raw_phi_root[16]     -2.45  1138 1.00
#> raw_phi_root[17]     -2.52  1160 1.00
#> raw_phi_root[18]     -2.58  1175 1.00
#> raw_phi_root[19]     -2.63  1172 1.00
#> raw_phi_root[20]     -2.68  1149 1.00
#> raw_phi_root[21]     -2.72  1116 1.00
#> raw_phi_root[22]     -2.75  1082 1.00
#> raw_phi_root[23]     -2.77  1055 1.00
#> raw_phi_root[24]     -2.79  1040 1.00
#> raw_phi_root[25]     -2.78  1045 1.00
#> raw_phi_root[26]     -2.77  1081 1.00
#> raw_phi_root[27]     -2.73  1149 1.00
#> raw_phi_root[28]     -2.66  1209 1.00
#> raw_phi_root[29]     -2.56  1186 1.00
#> raw_phi_root[30]     -2.41  1125 1.00
#> lp__             -78855.85   206 1.01
#> 
#> Samples were drawn using NUTS(diag_e) at Mon Oct  5 22:54:28 2026.
#> For each parameter, n_eff is a crude measure of effective sample size,
#> and Rhat is the potential scale reduction factor on split chains (at 
#> convergence, Rhat=1).
```

------------------------------------------------------------------------

### 3. Defining a Prediction Target Grid

To generate coverage predictions for specific populations, define a
target grid using
[`create_target()`](https://accidda.github.io/imuGAP/reference/create_target.md).

For example, to evaluate a cross-sectional “snapshot” across all
locations for ages 1 through 18 and doses 1 and 2:

``` r

target_grid <- create_target(
  location = unique(locations_sim$loc_id),
  age = 1:18,
  cohort = max(populations_sim$cohort) - 18,
  dose = c(1, 2),
  mode = "snapshot"
)
head(target_grid)
#>    obs_c_id               loc_id   age cohort  dose weight
#>       <int>               <char> <int>  <num> <num>  <num>
#> 1:        1                State     1     29     1      1
#> 2:        2              Scruggs     1     29     1      1
#> 3:        3               Simone     1     29     1      1
#> 4:        4               Watson     1     29     1      1
#> 5:        5 Chickadee Elementary     1     29     1      1
#> 6:        6     Nuthatch Academy     1     29     1      1
```

------------------------------------------------------------------------

### 4. Predicting Coverage

Pass the model fit and target grid to
[`predict()`](https://rdrr.io/r/stats/predict.html) to generate
posterior draws of coverage on the natural (probability) scale:

``` r

# Generate posterior draws for each target slice
predict_res <- predict(object = fit_sim, target = target_grid, posterior_size = 100)
```

You can load the pre-computed prediction fixture `predict_sim`:

``` r

data("predict_sim", package = "imuGAP")
predict_sim
#> An imuGAP predictions object (`imugap_predict`):
#>   Targets:   1008 target population slices across 28 locations
#>   Posterior: 100 draws (4 chains x 25 iterations)
#> 
#> Use summary() to compute quantiles or as.data.frame() to convert to a long table.
```

------------------------------------------------------------------------

### 5. Summarizing and Visualizing Predictions

You can compute vaccination coverage estimate summary statistics
(posterior mean, median, and credible intervals) on the natural
(probability) scale across your prediction targets using
[`summary()`](https://rdrr.io/r/base/summary.html):

``` r

summary_predict <- summary(predict_sim)

# Filter coverage estimate statistics to a specific target slice (e.g. age 5, dose 2)
subset(
  summary_predict,
  age == 5 & dose == 2,
  select = -c(age, dose, weight, cohort, obs_c_id, loc_c_id)
) |>
  print(row.names = FALSE)
#>                     loc_id      mean      q2_5       q50     q97_5
#>                     <char>     <num>     <num>     <num>     <num>
#>                      State 0.8938966 0.8914710 0.8937247 0.8964922
#>                    Scruggs 0.8955882 0.8925036 0.8955524 0.8982356
#>                     Simone 0.8713302 0.8658637 0.8709634 0.8763258
#>                     Watson 0.9094161 0.9058279 0.9093882 0.9125908
#>       Chickadee Elementary 0.9110313 0.9044839 0.9118564 0.9167646
#>           Nuthatch Academy 0.8990763 0.8948643 0.8988617 0.9031767
#>          Blue Heron School 0.9098535 0.9043493 0.9095600 0.9166187
#>      Flycatcher Elementary 0.9272374 0.9206117 0.9273772 0.9340173
#>   Bluebird Learning Center 0.9232094 0.9149482 0.9237226 0.9317511
#>            Catbird Academy 0.8860201 0.8814447 0.8859184 0.8900321
#>           Finch Elementary 0.8938708 0.8819487 0.8936284 0.9056622
#>             Sparrow School 0.9190316 0.9116940 0.9191386 0.9259604
#>  Towhee Children's Academy 0.8305163 0.8203889 0.8304751 0.8388064
#>         Warbler Elementary 0.8468077 0.8298707 0.8473260 0.8615293
#>           Egret Elementary 0.9154564 0.9100156 0.9155614 0.9210177
#>           Cardinal Academy 0.6508560 0.6214219 0.6491157 0.6789044
#>             Bunting School 0.7570317 0.7408511 0.7572205 0.7692015
#>            Tanager Academy 0.9315421 0.9230331 0.9316436 0.9391401
#>       Oriole Youth Academy 0.8510462 0.8441964 0.8507733 0.8600512
#>   Grosbeak Learning Center 0.6338940 0.6049421 0.6356173 0.6619217
#>           Junco Elementary 0.8755312 0.8681308 0.8754461 0.8819841
#>          Meadowlark School 0.8831387 0.8702399 0.8830218 0.8958156
#>       Goldfinch Elementary 0.9052839 0.8978532 0.9054394 0.9112988
#>        Mockingbird Academy 0.8866732 0.8765457 0.8868942 0.8962716
#>    Kinglet Learning Center 0.9317520 0.9260986 0.9316309 0.9361755
#>               Vireo School 0.9331278 0.9282268 0.9331771 0.9383217
#>         Kingfisher Academy 0.8780771 0.8677246 0.8783264 0.8887806
#>       Cormorant Elementary 0.9010061 0.8959919 0.9006413 0.9069406
#>                     loc_id      mean      q2_5       q50     q97_5
#>                     <char>     <num>     <num>     <num>     <num>
```

#### State-Level Coverage Trajectory

The following plot shows predicted state-level two-dose coverage across
ages 5 to 18 against the true simulation baseline:

**Show plot code**

``` r

data("latent_params_sim", package = "imuGAP")

state_predict <- summary_predict[loc_id == "State" & dose == 2 & age > 4]
state_idx <- predict_sim$target[loc_id == "State" & dose == 2 & age > 4, which = TRUE]
state_predict[, latent := latent_params_sim$coverage[state_idx]]

ggplot(state_predict) +
  aes(x = age) +
  geom_ribbon(aes(ymin = q2_5, ymax = q97_5, fill = "95% Credible Interval"), alpha = 0.25) +
  geom_line(aes(y = q50, color = "Posterior Median"), linewidth = 0.8) +
  geom_line(aes(y = latent, color = "True Latent"), linetype = "dashed", linewidth = 0.8) +
  scale_x_continuous(breaks = 5:18, minor_breaks = NULL) +
  coord_cartesian(ylim = c(0.8, 1.0)) +
  scale_color_manual(
    name = NULL,
    values = c("Posterior Median" = "black", "True Latent" = "firebrick")
  ) +
  scale_fill_manual(name = NULL, values = c("95% Credible Interval" = "grey50")) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.05, 0.05),
    legend.justification.inside = c(0, 0)
  ) +
  labs(x = "Age", y = "State-Level Two-Dose Coverage")
```

![](imuGAP_files/figure-html/state-viz-1.png)

------------------------------------------------------------------------

#### County-Level Estimates

The following plot compares county-level coverage estimates across
counties:

**Show plot code**

``` r

summary_predict |>
  subset(loc_id %in% c("Scruggs", "Simone", "Watson") & dose == 2 & age > 4) |>
  transform(loc_id = factor(loc_id, levels = c("Simone", "Watson", "Scruggs"))) |>
  ggplot() +
  aes(x = age) +
  geom_line(aes(y = q50, color = loc_id)) +
  geom_ribbon(aes(ymin = q2_5, ymax = q97_5, fill = loc_id), alpha = 0.2) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.12, 0.05),
    legend.justification.inside = c(0, 0)
  ) +
  scale_x_continuous(breaks = 5:18, minor_breaks = NULL) +
  coord_cartesian(ylim = c(0.8, 1.0)) +
  scale_color_discrete(NULL, aesthetics = c("color", "fill")) +
  labs(x = "Age", y = "County-Level Two-Dose Coverage")
```

![](imuGAP_files/figure-html/county-viz-1.png)

------------------------------------------------------------------------

#### Extracting Long-Format Draws for Custom Analysis

Use [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) to
extract individual MCMC draws in a tidy, long format:

``` r

draws_df <- as.data.frame(predict_sim)
head(draws_df)
#>    iteration chain obs_c_id loc_id   age cohort  dose weight loc_c_id coverage
#>        <int> <int>    <int> <char> <int>  <num> <num>  <num>    <int>    <num>
#> 1:         1     1        1  State     1     29     1      1        1        0
#> 2:         2     1        1  State     1     29     1      1        1        0
#> 3:         3     1        1  State     1     29     1      1        1        0
#> 4:         4     1        1  State     1     29     1      1        1        0
#> 5:         5     1        1  State     1     29     1      1        1        0
#> 6:         6     1        1  State     1     29     1      1        1        0
```

------------------------------------------------------------------------

## Summary and Guide to Other Vignettes

To explore advanced features and deeper technical details, see the
following companion vignettes:

- **[Included Example Datasets and Data
  Validation](https://accidda.github.io/imuGAP/articles/example_data.md)**:
  Detailed documentation of the bundled simulation fixtures,
  surveillance stream types (ChildVaxView, SchoolVaxView, TeenVaxView),
  canonicalization rules, and validation error diagnostics.

- **[Flexible Location Layers in
  imuGAP](https://accidda.github.io/imuGAP/articles/user_specified_layers.md)**:
  Demonstrates how `imuGAP` estimates the process model across 1-layer,
  2-layer, and multi-layer hierarchical population structures, showing
  how inferred relationships enable sub-regional estimation and cohort
  projection.

- **[Fit Inspection and Stan
  Diagnostics](https://accidda.github.io/imuGAP/articles/examining_fits.md)**:
  Comprehensive guide to MCMC convergence assessment, parameter
  extraction with
  [`extract_imugap()`](https://accidda.github.io/imuGAP/reference/extract_imugap.md),
  and parameter recovery trace plots for $`\beta_{\text{bs}}`$,
  $`\sigma_{\text{layer}}`$, $`\delta_{\text{loc}}`$, and $`\lambda`$.
