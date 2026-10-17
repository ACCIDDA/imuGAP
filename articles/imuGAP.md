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
#> beta_bs[1]           -1.68    0.00 0.03     -1.74     -1.70     -1.68     -1.65
#> beta_bs[2]           -1.88    0.00 0.05     -1.97     -1.91     -1.88     -1.84
#> beta_bs[3]           -2.48    0.00 0.08     -2.65     -2.54     -2.48     -2.43
#> beta_bs[4]           -3.12    0.00 0.09     -3.30     -3.18     -3.12     -3.06
#> beta_bs[5]           -2.59    0.00 0.09     -2.77     -2.65     -2.59     -2.53
#> sigma_layer[1]        0.53    0.01 0.29      0.18      0.31      0.45      0.66
#> sigma_layer[2]        0.75    0.01 0.13      0.54      0.66      0.74      0.83
#> lambda_raw[1]         1.02    0.00 0.03      0.97      1.00      1.02      1.04
#> lambda_raw[2]         1.06    0.00 0.01      1.03      1.05      1.06      1.07
#> raw_phi_root[1]      -1.68    0.00 0.03     -1.74     -1.70     -1.68     -1.65
#> raw_phi_root[2]      -1.72    0.00 0.03     -1.77     -1.74     -1.72     -1.70
#> raw_phi_root[3]      -1.77    0.00 0.02     -1.81     -1.78     -1.77     -1.75
#> raw_phi_root[4]      -1.81    0.00 0.02     -1.86     -1.83     -1.81     -1.80
#> raw_phi_root[5]      -1.86    0.00 0.03     -1.91     -1.88     -1.86     -1.85
#> raw_phi_root[6]      -1.92    0.00 0.03     -1.97     -1.93     -1.92     -1.90
#> raw_phi_root[7]      -1.97    0.00 0.03     -2.02     -1.99     -1.97     -1.95
#> raw_phi_root[8]      -2.03    0.00 0.03     -2.08     -2.05     -2.03     -2.01
#> raw_phi_root[9]      -2.09    0.00 0.03     -2.14     -2.10     -2.09     -2.07
#> raw_phi_root[10]     -2.15    0.00 0.03     -2.20     -2.16     -2.15     -2.13
#> raw_phi_root[11]     -2.21    0.00 0.03     -2.26     -2.23     -2.21     -2.19
#> raw_phi_root[12]     -2.27    0.00 0.03     -2.33     -2.29     -2.27     -2.25
#> raw_phi_root[13]     -2.33    0.00 0.03     -2.40     -2.35     -2.33     -2.31
#> raw_phi_root[14]     -2.39    0.00 0.03     -2.46     -2.42     -2.39     -2.37
#> raw_phi_root[15]     -2.46    0.00 0.04     -2.53     -2.48     -2.46     -2.43
#> raw_phi_root[16]     -2.52    0.00 0.04     -2.60     -2.55     -2.52     -2.50
#> raw_phi_root[17]     -2.59    0.00 0.04     -2.66     -2.61     -2.59     -2.56
#> raw_phi_root[18]     -2.65    0.00 0.04     -2.73     -2.67     -2.65     -2.62
#> raw_phi_root[19]     -2.71    0.00 0.04     -2.79     -2.73     -2.70     -2.68
#> raw_phi_root[20]     -2.76    0.00 0.04     -2.85     -2.79     -2.76     -2.73
#> raw_phi_root[21]     -2.80    0.00 0.05     -2.90     -2.83     -2.80     -2.77
#> raw_phi_root[22]     -2.84    0.00 0.05     -2.93     -2.87     -2.84     -2.80
#> raw_phi_root[23]     -2.86    0.00 0.05     -2.96     -2.89     -2.86     -2.83
#> raw_phi_root[24]     -2.87    0.00 0.05     -2.97     -2.91     -2.87     -2.84
#> raw_phi_root[25]     -2.87    0.00 0.05     -2.97     -2.90     -2.87     -2.84
#> raw_phi_root[26]     -2.85    0.00 0.05     -2.95     -2.89     -2.85     -2.82
#> raw_phi_root[27]     -2.82    0.00 0.05     -2.92     -2.85     -2.82     -2.79
#> raw_phi_root[28]     -2.76    0.00 0.05     -2.88     -2.80     -2.76     -2.73
#> raw_phi_root[29]     -2.69    0.00 0.07     -2.83     -2.73     -2.69     -2.64
#> raw_phi_root[30]     -2.59    0.00 0.09     -2.77     -2.65     -2.59     -2.53
#> lp__             -78864.90    0.32 5.28 -78875.91 -78868.14 -78864.46 -78861.20
#>                      97.5% n_eff Rhat
#> beta_bs[1]           -1.62  1271 1.00
#> beta_bs[2]           -1.78  1000 1.00
#> beta_bs[3]           -2.32   917 1.00
#> beta_bs[4]           -2.95   998 1.00
#> beta_bs[5]           -2.43   961 1.00
#> sigma_layer[1]        1.30   615 1.02
#> sigma_layer[2]        1.04   164 1.03
#> lambda_raw[1]         1.07  1227 1.00
#> lambda_raw[2]         1.09  1073 1.00
#> raw_phi_root[1]      -1.62  1271 1.00
#> raw_phi_root[2]      -1.67  1288 1.00
#> raw_phi_root[3]      -1.72  1229 1.00
#> raw_phi_root[4]      -1.77  1148 1.00
#> raw_phi_root[5]      -1.81  1110 1.00
#> raw_phi_root[6]      -1.87  1111 1.00
#> raw_phi_root[7]      -1.92  1135 1.00
#> raw_phi_root[8]      -1.98  1165 1.00
#> raw_phi_root[9]      -2.03  1186 1.00
#> raw_phi_root[10]     -2.09  1187 1.00
#> raw_phi_root[11]     -2.15  1171 1.00
#> raw_phi_root[12]     -2.21  1147 1.00
#> raw_phi_root[13]     -2.27  1123 1.00
#> raw_phi_root[14]     -2.33  1108 1.00
#> raw_phi_root[15]     -2.39  1113 1.00
#> raw_phi_root[16]     -2.45  1119 1.00
#> raw_phi_root[17]     -2.51  1116 1.00
#> raw_phi_root[18]     -2.57  1112 1.00
#> raw_phi_root[19]     -2.62  1104 1.00
#> raw_phi_root[20]     -2.67  1091 1.00
#> raw_phi_root[21]     -2.71  1077 1.00
#> raw_phi_root[22]     -2.74  1066 1.00
#> raw_phi_root[23]     -2.77  1060 1.00
#> raw_phi_root[24]     -2.78  1062 1.00
#> raw_phi_root[25]     -2.78  1079 1.00
#> raw_phi_root[26]     -2.76  1116 1.00
#> raw_phi_root[27]     -2.72  1173 1.00
#> raw_phi_root[28]     -2.66  1072 1.00
#> raw_phi_root[29]     -2.57  1002 1.00
#> raw_phi_root[30]     -2.43   961 1.00
#> lp__             -78855.71   278 1.03
#> 
#> Samples were drawn using NUTS(diag_e) at Sat Oct 17 13:43:03 2026.
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
#>                      State 0.8941202 0.8918940 0.8942558 0.8968028
#>                    Scruggs 0.8958313 0.8930288 0.8957341 0.8989630
#>                     Simone 0.8715769 0.8666465 0.8715377 0.8762501
#>                     Watson 0.9095794 0.9063210 0.9096535 0.9129749
#>       Chickadee Elementary 0.9113952 0.9070958 0.9112977 0.9168560
#>           Nuthatch Academy 0.8992920 0.8945334 0.8994987 0.9038578
#>          Blue Heron School 0.9103838 0.9038001 0.9108623 0.9163914
#>      Flycatcher Elementary 0.9279349 0.9200319 0.9280317 0.9348729
#>   Bluebird Learning Center 0.9227112 0.9154699 0.9228064 0.9296936
#>            Catbird Academy 0.8865567 0.8815253 0.8868469 0.8909103
#>           Finch Elementary 0.8922149 0.8800152 0.8932203 0.9034129
#>             Sparrow School 0.9187402 0.9109177 0.9188929 0.9245585
#>  Towhee Children's Academy 0.8300925 0.8229607 0.8292436 0.8401954
#>         Warbler Elementary 0.8479934 0.8305481 0.8480960 0.8683958
#>           Egret Elementary 0.9157294 0.9101608 0.9156578 0.9213708
#>           Cardinal Academy 0.6504556 0.6234627 0.6497227 0.6794939
#>             Bunting School 0.7584748 0.7428060 0.7590201 0.7745401
#>            Tanager Academy 0.9322319 0.9196674 0.9325119 0.9399215
#>       Oriole Youth Academy 0.8510426 0.8422411 0.8507695 0.8595885
#>   Grosbeak Learning Center 0.6325385 0.5984666 0.6339391 0.6636959
#>           Junco Elementary 0.8752691 0.8678595 0.8754308 0.8808996
#>          Meadowlark School 0.8832995 0.8696379 0.8838657 0.8948046
#>       Goldfinch Elementary 0.9056433 0.8982743 0.9057911 0.9116068
#>        Mockingbird Academy 0.8862321 0.8775538 0.8864936 0.8952222
#>    Kinglet Learning Center 0.9314907 0.9247321 0.9318270 0.9368521
#>               Vireo School 0.9339299 0.9267283 0.9341014 0.9399921
#>         Kingfisher Academy 0.8776989 0.8662536 0.8777660 0.8897087
#>       Cormorant Elementary 0.9010954 0.8937194 0.9012197 0.9062675
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
