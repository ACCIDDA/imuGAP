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
#> Inference for Stan model: bspline_static_zero_halley2_logit.
#> 4 chains, each with iter=125; warmup=0; thin=1; 
#> post-warmup draws per chain=125, total post-warmup draws=500.
#> 
#>                       mean se_mean   sd      2.5%       25%       50%       75%
#> beta_bs[1]           -1.64    0.00 0.03     -1.70     -1.66     -1.64     -1.62
#> beta_bs[2]           -1.84    0.00 0.05     -1.94     -1.88     -1.84     -1.81
#> beta_bs[3]           -2.44    0.00 0.09     -2.61     -2.50     -2.44     -2.38
#> beta_bs[4]           -3.10    0.00 0.09     -3.28     -3.15     -3.09     -3.04
#> beta_bs[5]           -2.56    0.01 0.09     -2.77     -2.62     -2.55     -2.49
#> sigma_layer[1]        0.68    0.03 0.38      0.23      0.40      0.57      0.85
#> sigma_layer[2]        0.84    0.01 0.14      0.62      0.74      0.82      0.92
#> lambda_raw[1]         1.04    0.00 0.03      0.99      1.02      1.04      1.06
#> lambda_raw[2]         1.03    0.00 0.01      1.01      1.02      1.03      1.04
#> raw_phi_root[1]      -1.64    0.00 0.03     -1.70     -1.66     -1.64     -1.62
#> raw_phi_root[2]      -1.68    0.00 0.02     -1.73     -1.70     -1.68     -1.67
#> raw_phi_root[3]      -1.73    0.00 0.02     -1.77     -1.75     -1.73     -1.72
#> raw_phi_root[4]      -1.78    0.00 0.02     -1.82     -1.79     -1.78     -1.76
#> raw_phi_root[5]      -1.83    0.00 0.02     -1.87     -1.85     -1.83     -1.81
#> raw_phi_root[6]      -1.88    0.00 0.02     -1.93     -1.90     -1.88     -1.86
#> raw_phi_root[7]      -1.93    0.00 0.02     -1.98     -1.95     -1.93     -1.92
#> raw_phi_root[8]      -1.99    0.00 0.02     -2.04     -2.01     -1.99     -1.97
#> raw_phi_root[9]      -2.05    0.00 0.03     -2.10     -2.07     -2.05     -2.03
#> raw_phi_root[10]     -2.11    0.00 0.03     -2.16     -2.13     -2.11     -2.09
#> raw_phi_root[11]     -2.17    0.00 0.03     -2.22     -2.19     -2.17     -2.15
#> raw_phi_root[12]     -2.23    0.00 0.03     -2.29     -2.25     -2.23     -2.21
#> raw_phi_root[13]     -2.29    0.00 0.03     -2.35     -2.31     -2.29     -2.27
#> raw_phi_root[14]     -2.36    0.00 0.03     -2.42     -2.38     -2.36     -2.34
#> raw_phi_root[15]     -2.42    0.00 0.03     -2.49     -2.44     -2.42     -2.40
#> raw_phi_root[16]     -2.49    0.00 0.03     -2.55     -2.51     -2.49     -2.46
#> raw_phi_root[17]     -2.55    0.00 0.04     -2.62     -2.57     -2.55     -2.53
#> raw_phi_root[18]     -2.61    0.00 0.04     -2.68     -2.64     -2.61     -2.59
#> raw_phi_root[19]     -2.67    0.00 0.04     -2.75     -2.70     -2.67     -2.64
#> raw_phi_root[20]     -2.72    0.00 0.04     -2.80     -2.75     -2.72     -2.70
#> raw_phi_root[21]     -2.77    0.00 0.04     -2.85     -2.80     -2.77     -2.74
#> raw_phi_root[22]     -2.80    0.00 0.05     -2.89     -2.84     -2.80     -2.77
#> raw_phi_root[23]     -2.83    0.00 0.05     -2.92     -2.86     -2.83     -2.80
#> raw_phi_root[24]     -2.84    0.00 0.05     -2.93     -2.88     -2.84     -2.81
#> raw_phi_root[25]     -2.84    0.00 0.05     -2.93     -2.87     -2.84     -2.81
#> raw_phi_root[26]     -2.82    0.00 0.05     -2.92     -2.86     -2.82     -2.79
#> raw_phi_root[27]     -2.79    0.00 0.05     -2.89     -2.82     -2.79     -2.75
#> raw_phi_root[28]     -2.73    0.00 0.06     -2.86     -2.77     -2.73     -2.69
#> raw_phi_root[29]     -2.66    0.00 0.07     -2.82     -2.70     -2.65     -2.61
#> raw_phi_root[30]     -2.56    0.01 0.09     -2.77     -2.62     -2.55     -2.49
#> mu_offset[1,1]       -0.04    0.00 0.00     -0.04     -0.04     -0.04     -0.03
#> mu_offset[1,2]       -0.11    0.00 0.01     -0.15     -0.12     -0.11     -0.10
#> mu_offset[1,3]       -0.21    0.00 0.03     -0.29     -0.23     -0.21     -0.19
#> mu_offset[1,4]       -0.21    0.00 0.06     -0.35     -0.25     -0.20     -0.17
#> mu_offset[2,1]       -0.04    0.00 0.00     -0.05     -0.04     -0.04     -0.03
#> mu_offset[2,2]       -0.11    0.00 0.01     -0.15     -0.12     -0.11     -0.10
#> mu_offset[2,3]       -0.22    0.00 0.03     -0.29     -0.24     -0.21     -0.20
#> mu_offset[2,4]       -0.21    0.00 0.06     -0.35     -0.25     -0.20     -0.17
#> mu_offset[3,1]       -0.04    0.00 0.00     -0.05     -0.04     -0.04     -0.04
#> mu_offset[3,2]       -0.12    0.00 0.02     -0.15     -0.13     -0.12     -0.11
#> mu_offset[3,3]       -0.23    0.00 0.03     -0.30     -0.24     -0.22     -0.20
#> mu_offset[3,4]       -0.21    0.00 0.06     -0.35     -0.25     -0.21     -0.17
#> mu_offset[4,1]       -0.04    0.00 0.00     -0.05     -0.04     -0.04     -0.04
#> mu_offset[4,2]       -0.12    0.00 0.02     -0.15     -0.13     -0.12     -0.11
#> mu_offset[4,3]       -0.23    0.00 0.03     -0.31     -0.25     -0.23     -0.21
#> mu_offset[4,4]       -0.22    0.00 0.06     -0.35     -0.25     -0.21     -0.17
#> mu_offset[5,1]       -0.04    0.00 0.00     -0.05     -0.04     -0.04     -0.04
#> mu_offset[5,2]       -0.12    0.00 0.02     -0.15     -0.13     -0.12     -0.11
#> mu_offset[5,3]       -0.24    0.00 0.03     -0.31     -0.26     -0.24     -0.22
#> mu_offset[5,4]       -0.22    0.00 0.06     -0.35     -0.25     -0.21     -0.17
#> mu_offset[6,1]       -0.04    0.00 0.00     -0.05     -0.04     -0.04     -0.04
#> mu_offset[6,2]       -0.12    0.00 0.02     -0.16     -0.13     -0.12     -0.11
#> mu_offset[6,3]       -0.25    0.00 0.03     -0.32     -0.27     -0.24     -0.22
#> mu_offset[6,4]       -0.22    0.00 0.06     -0.36     -0.25     -0.21     -0.17
#> mu_offset[7,1]       -0.04    0.00 0.00     -0.05     -0.04     -0.04     -0.04
#> mu_offset[7,2]       -0.12    0.00 0.02     -0.16     -0.13     -0.12     -0.11
#> mu_offset[7,3]       -0.25    0.00 0.03     -0.33     -0.27     -0.25     -0.23
#> mu_offset[7,4]       -0.22    0.00 0.06     -0.36     -0.26     -0.21     -0.18
#> mu_offset[8,1]       -0.04    0.00 0.00     -0.05     -0.04     -0.04     -0.04
#> mu_offset[8,2]       -0.13    0.00 0.02     -0.16     -0.14     -0.13     -0.12
#> mu_offset[8,3]       -0.26    0.00 0.03     -0.34     -0.28     -0.26     -0.24
#> mu_offset[8,4]       -0.22    0.00 0.06     -0.36     -0.26     -0.21     -0.18
#> mu_offset[9,1]       -0.04    0.00 0.00     -0.05     -0.05     -0.04     -0.04
#> mu_offset[9,2]       -0.13    0.00 0.02     -0.16     -0.14     -0.13     -0.12
#> mu_offset[9,3]       -0.27    0.00 0.03     -0.34     -0.29     -0.27     -0.25
#> mu_offset[9,4]       -0.22    0.00 0.06     -0.36     -0.26     -0.21     -0.18
#> mu_offset[10,1]      -0.04    0.00 0.00     -0.05     -0.05     -0.04     -0.04
#> mu_offset[10,2]      -0.13    0.00 0.02     -0.17     -0.14     -0.13     -0.12
#> mu_offset[10,3]      -0.28    0.00 0.03     -0.35     -0.30     -0.27     -0.25
#> mu_offset[10,4]      -0.22    0.00 0.06     -0.36     -0.26     -0.21     -0.18
#> mu_offset[11,1]      -0.04    0.00 0.00     -0.05     -0.05     -0.04     -0.04
#> mu_offset[11,2]      -0.13    0.00 0.02     -0.17     -0.14     -0.13     -0.12
#> mu_offset[11,3]      -0.28    0.00 0.03     -0.36     -0.30     -0.28     -0.26
#> mu_offset[11,4]      -0.22    0.00 0.06     -0.36     -0.26     -0.22     -0.18
#> mu_offset[12,1]      -0.04    0.00 0.00     -0.05     -0.05     -0.04     -0.04
#> mu_offset[12,2]      -0.13    0.00 0.02     -0.17     -0.14     -0.13     -0.12
#> mu_offset[12,3]      -0.29    0.00 0.03     -0.37     -0.31     -0.29     -0.27
#> mu_offset[12,4]      -0.23    0.00 0.06     -0.37     -0.26     -0.22     -0.18
#> mu_offset[13,1]      -0.05    0.00 0.00     -0.05     -0.05     -0.05     -0.04
#> mu_offset[13,2]      -0.14    0.00 0.02     -0.17     -0.15     -0.13     -0.12
#> mu_offset[13,3]      -0.30    0.00 0.03     -0.37     -0.32     -0.29     -0.27
#> mu_offset[13,4]      -0.23    0.00 0.06     -0.37     -0.26     -0.22     -0.18
#> mu_offset[14,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.04
#> mu_offset[14,2]      -0.14    0.00 0.02     -0.17     -0.15     -0.14     -0.12
#> mu_offset[14,3]      -0.30    0.00 0.03     -0.38     -0.33     -0.30     -0.28
#> mu_offset[14,4]      -0.23    0.00 0.06     -0.37     -0.26     -0.22     -0.18
#> mu_offset[15,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.04
#> mu_offset[15,2]      -0.14    0.00 0.02     -0.18     -0.15     -0.14     -0.13
#> mu_offset[15,3]      -0.31    0.00 0.03     -0.39     -0.33     -0.31     -0.29
#> mu_offset[15,4]      -0.23    0.00 0.06     -0.37     -0.27     -0.22     -0.18
#> mu_offset[16,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.04
#> mu_offset[16,2]      -0.14    0.00 0.02     -0.18     -0.15     -0.14     -0.13
#> mu_offset[16,3]      -0.32    0.00 0.03     -0.40     -0.34     -0.31     -0.29
#> mu_offset[16,4]      -0.23    0.00 0.06     -0.37     -0.27     -0.22     -0.18
#> mu_offset[17,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.04
#> mu_offset[17,2]      -0.14    0.00 0.02     -0.18     -0.15     -0.14     -0.13
#> mu_offset[17,3]      -0.32    0.00 0.04     -0.40     -0.34     -0.32     -0.30
#> mu_offset[17,4]      -0.23    0.00 0.06     -0.37     -0.27     -0.22     -0.19
#> mu_offset[18,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.04
#> mu_offset[18,2]      -0.14    0.00 0.02     -0.18     -0.15     -0.14     -0.13
#> mu_offset[18,3]      -0.33    0.00 0.04     -0.41     -0.35     -0.33     -0.30
#> mu_offset[18,4]      -0.23    0.00 0.06     -0.37     -0.27     -0.22     -0.19
#> mu_offset[19,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.05
#> mu_offset[19,2]      -0.14    0.00 0.02     -0.18     -0.15     -0.14     -0.13
#> mu_offset[19,3]      -0.33    0.00 0.04     -0.41     -0.36     -0.33     -0.31
#> mu_offset[19,4]      -0.23    0.00 0.06     -0.37     -0.27     -0.22     -0.19
#> mu_offset[20,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.05
#> mu_offset[20,2]      -0.14    0.00 0.02     -0.18     -0.16     -0.14     -0.13
#> mu_offset[20,3]      -0.34    0.00 0.04     -0.42     -0.36     -0.34     -0.31
#> mu_offset[20,4]      -0.23    0.00 0.06     -0.37     -0.27     -0.22     -0.19
#> mu_offset[21,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.05
#> mu_offset[21,2]      -0.15    0.00 0.02     -0.18     -0.16     -0.14     -0.13
#> mu_offset[21,3]      -0.34    0.00 0.04     -0.42     -0.36     -0.34     -0.32
#> mu_offset[21,4]      -0.23    0.00 0.06     -0.37     -0.27     -0.22     -0.19
#> mu_offset[22,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.05
#> mu_offset[22,2]      -0.15    0.00 0.02     -0.18     -0.16     -0.15     -0.13
#> mu_offset[22,3]      -0.34    0.00 0.04     -0.43     -0.37     -0.34     -0.32
#> mu_offset[22,4]      -0.23    0.00 0.06     -0.37     -0.27     -0.22     -0.19
#> mu_offset[23,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.05
#> mu_offset[23,2]      -0.15    0.00 0.02     -0.19     -0.16     -0.15     -0.13
#> mu_offset[23,3]      -0.35    0.00 0.04     -0.43     -0.37     -0.34     -0.32
#> mu_offset[23,4]      -0.23    0.00 0.06     -0.37     -0.27     -0.22     -0.19
#> mu_offset[24,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.05
#> mu_offset[24,2]      -0.15    0.00 0.02     -0.19     -0.16     -0.15     -0.13
#> mu_offset[24,3]      -0.35    0.00 0.04     -0.43     -0.37     -0.34     -0.32
#> mu_offset[24,4]      -0.23    0.00 0.06     -0.37     -0.27     -0.22     -0.19
#> mu_offset[25,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.05
#> mu_offset[25,2]      -0.15    0.00 0.02     -0.19     -0.16     -0.15     -0.13
#> mu_offset[25,3]      -0.35    0.00 0.04     -0.43     -0.37     -0.34     -0.32
#> mu_offset[25,4]      -0.23    0.00 0.06     -0.37     -0.27     -0.22     -0.19
#> mu_offset[26,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.05
#> mu_offset[26,2]      -0.15    0.00 0.02     -0.18     -0.16     -0.15     -0.13
#> mu_offset[26,3]      -0.35    0.00 0.04     -0.43     -0.37     -0.34     -0.32
#> mu_offset[26,4]      -0.23    0.00 0.06     -0.37     -0.27     -0.22     -0.19
#> mu_offset[27,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.05
#> mu_offset[27,2]      -0.15    0.00 0.02     -0.18     -0.16     -0.14     -0.13
#> mu_offset[27,3]      -0.34    0.00 0.04     -0.42     -0.37     -0.34     -0.32
#> mu_offset[27,4]      -0.23    0.00 0.06     -0.37     -0.27     -0.22     -0.19
#> mu_offset[28,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.05
#> mu_offset[28,2]      -0.14    0.00 0.02     -0.18     -0.16     -0.14     -0.13
#> mu_offset[28,3]      -0.34    0.00 0.04     -0.42     -0.36     -0.34     -0.31
#> mu_offset[28,4]      -0.23    0.00 0.06     -0.37     -0.27     -0.22     -0.19
#> mu_offset[29,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.05
#> mu_offset[29,2]      -0.14    0.00 0.02     -0.18     -0.15     -0.14     -0.13
#> mu_offset[29,3]      -0.33    0.00 0.04     -0.41     -0.36     -0.33     -0.31
#> mu_offset[29,4]      -0.23    0.00 0.06     -0.37     -0.27     -0.22     -0.19
#> mu_offset[30,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.04
#> mu_offset[30,2]      -0.14    0.00 0.02     -0.18     -0.15     -0.14     -0.13
#> mu_offset[30,3]      -0.32    0.00 0.04     -0.40     -0.35     -0.32     -0.30
#> mu_offset[30,4]      -0.23    0.00 0.06     -0.37     -0.27     -0.22     -0.19
#> lp__             -78912.30    0.28 4.82 -78922.73 -78915.16 -78911.96 -78908.91
#>                      97.5% n_eff Rhat
#> beta_bs[1]           -1.58  1124 1.00
#> beta_bs[2]           -1.74   909 1.01
#> beta_bs[3]           -2.28   747 1.00
#> beta_bs[4]           -2.92   993 1.00
#> beta_bs[5]           -2.39   348 1.01
#> sigma_layer[1]        1.71   127 1.05
#> sigma_layer[2]        1.17   240 1.01
#> lambda_raw[1]         1.09   817 1.01
#> lambda_raw[2]         1.06   520 1.00
#> raw_phi_root[1]      -1.58  1124 1.00
#> raw_phi_root[2]      -1.64  1008 1.00
#> raw_phi_root[3]      -1.69   828 1.00
#> raw_phi_root[4]      -1.73   706 1.01
#> raw_phi_root[5]      -1.78   660 1.01
#> raw_phi_root[6]      -1.83   634 1.01
#> raw_phi_root[7]      -1.89   603 1.01
#> raw_phi_root[8]      -1.94   571 1.01
#> raw_phi_root[9]      -2.00   546 1.01
#> raw_phi_root[10]     -2.06   535 1.01
#> raw_phi_root[11]     -2.12   546 1.01
#> raw_phi_root[12]     -2.18   571 1.00
#> raw_phi_root[13]     -2.24   606 1.00
#> raw_phi_root[14]     -2.30   614 1.00
#> raw_phi_root[15]     -2.36   625 1.00
#> raw_phi_root[16]     -2.42   643 1.00
#> raw_phi_root[17]     -2.48   675 1.00
#> raw_phi_root[18]     -2.54   713 1.00
#> raw_phi_root[19]     -2.60   752 1.00
#> raw_phi_root[20]     -2.64   783 1.00
#> raw_phi_root[21]     -2.68   803 1.00
#> raw_phi_root[22]     -2.71   802 1.00
#> raw_phi_root[23]     -2.74   771 1.00
#> raw_phi_root[24]     -2.75   710 1.00
#> raw_phi_root[25]     -2.75   621 1.00
#> raw_phi_root[26]     -2.73   517 1.00
#> raw_phi_root[27]     -2.70   406 1.00
#> raw_phi_root[28]     -2.63   335 1.01
#> raw_phi_root[29]     -2.52   320 1.01
#> raw_phi_root[30]     -2.39   348 1.01
#> mu_offset[1,1]       -0.03   980 1.00
#> mu_offset[1,2]       -0.09  1062 1.00
#> mu_offset[1,3]       -0.16   585 1.00
#> mu_offset[1,4]       -0.12   419 1.01
#> mu_offset[2,1]       -0.03   991 1.00
#> mu_offset[2,2]       -0.09  1052 1.00
#> mu_offset[2,3]       -0.17   620 1.00
#> mu_offset[2,4]       -0.12   419 1.01
#> mu_offset[3,1]       -0.03  1003 1.00
#> mu_offset[3,2]       -0.09  1046 1.00
#> mu_offset[3,3]       -0.18   649 1.00
#> mu_offset[3,4]       -0.12   419 1.01
#> mu_offset[4,1]       -0.03  1012 1.00
#> mu_offset[4,2]       -0.09  1041 1.00
#> mu_offset[4,3]       -0.18   671 1.00
#> mu_offset[4,4]       -0.12   419 1.01
#> mu_offset[5,1]       -0.03  1017 1.00
#> mu_offset[5,2]       -0.09  1037 1.00
#> mu_offset[5,3]       -0.19   686 1.00
#> mu_offset[5,4]       -0.12   419 1.01
#> mu_offset[6,1]       -0.03  1019 1.00
#> mu_offset[6,2]       -0.10  1034 1.00
#> mu_offset[6,3]       -0.20   694 1.00
#> mu_offset[6,4]       -0.13   419 1.01
#> mu_offset[7,1]       -0.03  1018 1.00
#> mu_offset[7,2]       -0.10  1032 1.00
#> mu_offset[7,3]       -0.20   696 1.00
#> mu_offset[7,4]       -0.13   419 1.01
#> mu_offset[8,1]       -0.03  1017 1.00
#> mu_offset[8,2]       -0.10  1031 1.00
#> mu_offset[8,3]       -0.21   695 1.00
#> mu_offset[8,4]       -0.13   419 1.01
#> mu_offset[9,1]       -0.03  1014 1.00
#> mu_offset[9,2]       -0.10  1029 1.00
#> mu_offset[9,3]       -0.22   691 1.00
#> mu_offset[9,4]       -0.13   419 1.01
#> mu_offset[10,1]      -0.04  1012 1.00
#> mu_offset[10,2]      -0.10  1027 1.00
#> mu_offset[10,3]      -0.22   686 1.00
#> mu_offset[10,4]      -0.13   419 1.01
#> mu_offset[11,1]      -0.04  1010 1.00
#> mu_offset[11,2]      -0.10  1026 1.00
#> mu_offset[11,3]      -0.23   681 1.00
#> mu_offset[11,4]      -0.13   419 1.01
#> mu_offset[12,1]      -0.04  1010 1.00
#> mu_offset[12,2]      -0.11  1024 1.00
#> mu_offset[12,3]      -0.24   677 1.00
#> mu_offset[12,4]      -0.13   419 1.01
#> mu_offset[13,1]      -0.04  1010 1.00
#> mu_offset[13,2]      -0.11  1022 1.00
#> mu_offset[13,3]      -0.24   675 1.00
#> mu_offset[13,4]      -0.13   419 1.01
#> mu_offset[14,1]      -0.04  1012 1.00
#> mu_offset[14,2]      -0.11  1021 1.00
#> mu_offset[14,3]      -0.25   675 1.00
#> mu_offset[14,4]      -0.13   419 1.01
#> mu_offset[15,1]      -0.04  1016 1.00
#> mu_offset[15,2]      -0.11  1020 1.00
#> mu_offset[15,3]      -0.25   677 1.00
#> mu_offset[15,4]      -0.13   420 1.01
#> mu_offset[16,1]      -0.04  1021 1.00
#> mu_offset[16,2]      -0.11  1019 1.00
#> mu_offset[16,3]      -0.26   681 1.00
#> mu_offset[16,4]      -0.13   420 1.01
#> mu_offset[17,1]      -0.04  1028 1.00
#> mu_offset[17,2]      -0.11  1018 1.00
#> mu_offset[17,3]      -0.27   689 1.00
#> mu_offset[17,4]      -0.13   420 1.01
#> mu_offset[18,1]      -0.04  1035 1.00
#> mu_offset[18,2]      -0.11  1017 1.00
#> mu_offset[18,3]      -0.27   697 1.00
#> mu_offset[18,4]      -0.14   420 1.01
#> mu_offset[19,1]      -0.04  1041 1.00
#> mu_offset[19,2]      -0.11  1016 1.00
#> mu_offset[19,3]      -0.27   706 1.00
#> mu_offset[19,4]      -0.14   421 1.01
#> mu_offset[20,1]      -0.04  1046 1.00
#> mu_offset[20,2]      -0.12  1016 1.00
#> mu_offset[20,3]      -0.28   713 1.00
#> mu_offset[20,4]      -0.14   421 1.01
#> mu_offset[21,1]      -0.04  1050 1.00
#> mu_offset[21,2]      -0.12  1015 1.00
#> mu_offset[21,3]      -0.28   719 1.00
#> mu_offset[21,4]      -0.14   421 1.01
#> mu_offset[22,1]      -0.04  1052 1.00
#> mu_offset[22,2]      -0.12  1014 1.00
#> mu_offset[22,3]      -0.28   722 1.00
#> mu_offset[22,4]      -0.14   421 1.01
#> mu_offset[23,1]      -0.04  1051 1.00
#> mu_offset[23,2]      -0.12  1013 1.00
#> mu_offset[23,3]      -0.29   722 1.00
#> mu_offset[23,4]      -0.14   421 1.01
#> mu_offset[24,1]      -0.04  1047 1.00
#> mu_offset[24,2]      -0.12  1012 1.00
#> mu_offset[24,3]      -0.29   719 1.00
#> mu_offset[24,4]      -0.14   421 1.01
#> mu_offset[25,1]      -0.04  1040 1.00
#> mu_offset[25,2]      -0.12  1011 1.00
#> mu_offset[25,3]      -0.29   710 1.00
#> mu_offset[25,4]      -0.14   420 1.01
#> mu_offset[26,1]      -0.04  1028 1.00
#> mu_offset[26,2]      -0.12  1009 1.00
#> mu_offset[26,3]      -0.28   697 1.00
#> mu_offset[26,4]      -0.14   420 1.01
#> mu_offset[27,1]      -0.04  1010 1.00
#> mu_offset[27,2]      -0.12  1006 1.00
#> mu_offset[27,3]      -0.28   677 1.00
#> mu_offset[27,4]      -0.14   419 1.01
#> mu_offset[28,1]      -0.04   985 1.00
#> mu_offset[28,2]      -0.12  1003 1.00
#> mu_offset[28,3]      -0.28   649 1.00
#> mu_offset[28,4]      -0.14   418 1.01
#> mu_offset[29,1]      -0.04   951 1.00
#> mu_offset[29,2]      -0.11   998 1.00
#> mu_offset[29,3]      -0.27   620 1.00
#> mu_offset[29,4]      -0.14   416 1.01
#> mu_offset[30,1]      -0.04   904 1.00
#> mu_offset[30,2]      -0.11   992 1.00
#> mu_offset[30,3]      -0.26   587 1.00
#> mu_offset[30,4]      -0.13   414 1.01
#> lp__             -78903.91   289 1.01
#> 
#> Samples were drawn using NUTS(diag_e) at Thu Nov  5 15:39:25 2026.
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
#>                      State 0.8880614 0.8853131 0.8881675 0.8902876
#>                    Scruggs 0.8941627 0.8907439 0.8942479 0.8965081
#>                     Simone 0.8603580 0.8556576 0.8604063 0.8636991
#>                     Watson 0.9063655 0.9034245 0.9064080 0.9096999
#>       Chickadee Elementary 0.9128457 0.9082150 0.9128577 0.9178946
#>           Nuthatch Academy 0.9027791 0.8981103 0.9028886 0.9066022
#>          Blue Heron School 0.9126785 0.9065719 0.9126261 0.9178788
#>      Flycatcher Elementary 0.9280941 0.9205290 0.9286899 0.9345857
#>   Bluebird Learning Center 0.9248678 0.9165714 0.9252206 0.9325780
#>            Catbird Academy 0.8910720 0.8869016 0.8911371 0.8944337
#>           Finch Elementary 0.8981391 0.8833002 0.8989971 0.9114247
#>             Sparrow School 0.9204964 0.9133370 0.9206206 0.9271564
#>  Towhee Children's Academy 0.8395897 0.8320176 0.8397470 0.8466690
#>         Warbler Elementary 0.8565347 0.8437961 0.8564714 0.8717020
#>           Egret Elementary 0.9191998 0.9143917 0.9193054 0.9238874
#>           Cardinal Academy 0.6815103 0.6483983 0.6832811 0.7118517
#>             Bunting School 0.7789260 0.7667645 0.7783346 0.7899048
#>            Tanager Academy 0.9315033 0.9243967 0.9315792 0.9383919
#>       Oriole Youth Academy 0.8626906 0.8547990 0.8626803 0.8689689
#>   Grosbeak Learning Center 0.6629660 0.6337394 0.6617355 0.6889755
#>           Junco Elementary 0.8843433 0.8776638 0.8846550 0.8901398
#>          Meadowlark School 0.8873665 0.8779836 0.8869566 0.8982681
#>       Goldfinch Elementary 0.9081302 0.9005342 0.9084635 0.9131949
#>        Mockingbird Academy 0.8907236 0.8813495 0.8903434 0.8995441
#>    Kinglet Learning Center 0.9317875 0.9256221 0.9321348 0.9363635
#>               Vireo School 0.9332854 0.9288625 0.9329808 0.9385951
#>         Kingfisher Academy 0.8829680 0.8758425 0.8831255 0.8908596
#>       Cormorant Elementary 0.9042804 0.9001288 0.9043472 0.9086254
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
