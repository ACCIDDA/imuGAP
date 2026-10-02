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
#>                     mean se_mean   sd      2.5%       25%       50%       75%
#> beta_bs[1]         -1.68    0.00 0.03     -1.74     -1.70     -1.68     -1.65
#> beta_bs[2]         -1.88    0.00 0.05     -1.98     -1.91     -1.88     -1.85
#> beta_bs[3]         -2.48    0.00 0.08     -2.65     -2.53     -2.48     -2.42
#> beta_bs[4]         -3.13    0.00 0.09     -3.31     -3.19     -3.12     -3.06
#> beta_bs[5]         -2.59    0.00 0.09     -2.77     -2.65     -2.59     -2.53
#> sigma_layer[1]      0.54    0.01 0.28      0.20      0.33      0.47      0.68
#> sigma_layer[2]      0.73    0.01 0.12      0.54      0.65      0.72      0.80
#> lambda_raw[1]       1.02    0.00 0.03      0.97      1.00      1.02      1.04
#> lambda_raw[2]       1.06    0.00 0.01      1.03      1.05      1.06      1.07
#> lp__           -78865.17    0.30 5.17 -78876.68 -78868.33 -78864.81 -78861.51
#>                    97.5% n_eff Rhat
#> beta_bs[1]         -1.62  1629 1.00
#> beta_bs[2]         -1.78  1195 1.00
#> beta_bs[3]         -2.32  1222 1.00
#> beta_bs[4]         -2.96  1062 1.00
#> beta_bs[5]         -2.42  1751 1.00
#> sigma_layer[1]      1.25   719 1.00
#> sigma_layer[2]      1.03   204 1.05
#> lambda_raw[1]       1.07  1656 1.00
#> lambda_raw[2]       1.09  1222 1.00
#> lp__           -78855.94   303 1.03
#> 
#> Samples were drawn using NUTS(diag_e) at Fri Oct  2 14:40:11 2026.
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
#>                      State 0.8937532 0.8917752 0.8935799 0.8965204
#>                    Scruggs 0.8953011 0.8930026 0.8951455 0.8981017
#>                     Simone 0.8715545 0.8656466 0.8714774 0.8768219
#>                     Watson 0.9093453 0.9064332 0.9093152 0.9130300
#>       Chickadee Elementary 0.9104249 0.9038840 0.9105810 0.9158894
#>           Nuthatch Academy 0.8991954 0.8958546 0.8992796 0.9029854
#>          Blue Heron School 0.9098140 0.9029678 0.9098632 0.9161555
#>      Flycatcher Elementary 0.9275454 0.9219800 0.9272048 0.9342030
#>   Bluebird Learning Center 0.9226153 0.9139163 0.9228108 0.9304542
#>            Catbird Academy 0.8858356 0.8811738 0.8860899 0.8904062
#>           Finch Elementary 0.8924869 0.8754141 0.8929719 0.9047496
#>             Sparrow School 0.9185222 0.9112856 0.9182099 0.9253984
#>  Towhee Children's Academy 0.8291879 0.8223283 0.8292055 0.8382984
#>         Warbler Elementary 0.8481992 0.8287248 0.8491705 0.8634285
#>           Egret Elementary 0.9150607 0.9094418 0.9151663 0.9201529
#>           Cardinal Academy 0.6536101 0.6162001 0.6542225 0.6874443
#>             Bunting School 0.7555316 0.7389231 0.7553211 0.7713316
#>            Tanager Academy 0.9331258 0.9225213 0.9338355 0.9404860
#>       Oriole Youth Academy 0.8511857 0.8427624 0.8513959 0.8612966
#>   Grosbeak Learning Center 0.6338675 0.6040332 0.6333394 0.6598722
#>           Junco Elementary 0.8758110 0.8696818 0.8759998 0.8816810
#>          Meadowlark School 0.8830822 0.8721095 0.8835664 0.8927760
#>       Goldfinch Elementary 0.9056493 0.8989364 0.9057133 0.9117414
#>        Mockingbird Academy 0.8857986 0.8765183 0.8856001 0.8938231
#>    Kinglet Learning Center 0.9314687 0.9245190 0.9314427 0.9375068
#>               Vireo School 0.9335920 0.9269655 0.9336678 0.9401709
#>         Kingfisher Academy 0.8768774 0.8685692 0.8770229 0.8843303
#>       Cormorant Elementary 0.9009528 0.8954577 0.9010571 0.9069089
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
