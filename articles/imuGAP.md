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
#> beta_bs[1]           -1.64    0.00 0.03     -1.71     -1.66     -1.64     -1.62
#> beta_bs[2]           -1.84    0.00 0.05     -1.95     -1.88     -1.84     -1.80
#> beta_bs[3]           -2.44    0.00 0.09     -2.62     -2.50     -2.44     -2.38
#> beta_bs[4]           -3.09    0.00 0.10     -3.30     -3.16     -3.09     -3.03
#> beta_bs[5]           -2.56    0.00 0.09     -2.73     -2.62     -2.56     -2.49
#> sigma_layer[1]        0.65    0.01 0.32      0.24      0.39      0.57      0.82
#> sigma_layer[2]        0.84    0.01 0.16      0.59      0.72      0.81      0.92
#> lambda_raw[1]         1.04    0.00 0.03      0.99      1.02      1.04      1.06
#> lambda_raw[2]         1.03    0.00 0.01      1.01      1.02      1.03      1.04
#> raw_phi_root[1]      -1.64    0.00 0.03     -1.71     -1.66     -1.64     -1.62
#> raw_phi_root[2]      -1.68    0.00 0.02     -1.74     -1.70     -1.68     -1.67
#> raw_phi_root[3]      -1.73    0.00 0.02     -1.78     -1.74     -1.73     -1.71
#> raw_phi_root[4]      -1.78    0.00 0.02     -1.82     -1.79     -1.78     -1.76
#> raw_phi_root[5]      -1.83    0.00 0.02     -1.88     -1.84     -1.83     -1.81
#> raw_phi_root[6]      -1.88    0.00 0.03     -1.93     -1.90     -1.88     -1.86
#> raw_phi_root[7]      -1.93    0.00 0.03     -1.98     -1.95     -1.93     -1.92
#> raw_phi_root[8]      -1.99    0.00 0.03     -2.04     -2.01     -1.99     -1.97
#> raw_phi_root[9]      -2.05    0.00 0.03     -2.10     -2.06     -2.05     -2.03
#> raw_phi_root[10]     -2.11    0.00 0.03     -2.16     -2.12     -2.11     -2.09
#> raw_phi_root[11]     -2.17    0.00 0.03     -2.22     -2.19     -2.17     -2.15
#> raw_phi_root[12]     -2.23    0.00 0.03     -2.29     -2.25     -2.23     -2.21
#> raw_phi_root[13]     -2.29    0.00 0.03     -2.35     -2.31     -2.29     -2.27
#> raw_phi_root[14]     -2.36    0.00 0.03     -2.42     -2.38     -2.36     -2.33
#> raw_phi_root[15]     -2.42    0.00 0.03     -2.49     -2.44     -2.42     -2.40
#> raw_phi_root[16]     -2.49    0.00 0.03     -2.56     -2.51     -2.49     -2.46
#> raw_phi_root[17]     -2.55    0.00 0.04     -2.62     -2.57     -2.55     -2.53
#> raw_phi_root[18]     -2.61    0.00 0.04     -2.69     -2.64     -2.61     -2.59
#> raw_phi_root[19]     -2.67    0.00 0.04     -2.75     -2.70     -2.67     -2.64
#> raw_phi_root[20]     -2.72    0.00 0.04     -2.81     -2.75     -2.72     -2.69
#> raw_phi_root[21]     -2.77    0.00 0.05     -2.86     -2.80     -2.77     -2.74
#> raw_phi_root[22]     -2.80    0.00 0.05     -2.90     -2.84     -2.80     -2.77
#> raw_phi_root[23]     -2.83    0.00 0.05     -2.93     -2.86     -2.83     -2.79
#> raw_phi_root[24]     -2.84    0.00 0.05     -2.94     -2.87     -2.84     -2.81
#> raw_phi_root[25]     -2.84    0.00 0.05     -2.94     -2.87     -2.84     -2.81
#> raw_phi_root[26]     -2.82    0.00 0.05     -2.92     -2.85     -2.82     -2.79
#> raw_phi_root[27]     -2.79    0.00 0.05     -2.89     -2.82     -2.79     -2.76
#> raw_phi_root[28]     -2.73    0.00 0.05     -2.84     -2.77     -2.73     -2.69
#> raw_phi_root[29]     -2.66    0.00 0.07     -2.79     -2.70     -2.65     -2.61
#> raw_phi_root[30]     -2.56    0.00 0.09     -2.73     -2.62     -2.56     -2.49
#> mu_offset[1,1]       -0.04    0.00 0.00     -0.04     -0.04     -0.04     -0.03
#> mu_offset[1,2]       -0.11    0.00 0.02     -0.15     -0.12     -0.11     -0.10
#> mu_offset[1,3]       -0.21    0.00 0.03     -0.29     -0.23     -0.21     -0.19
#> mu_offset[1,4]       -0.21    0.00 0.06     -0.37     -0.24     -0.20     -0.17
#> mu_offset[2,1]       -0.04    0.00 0.00     -0.05     -0.04     -0.04     -0.03
#> mu_offset[2,2]       -0.11    0.00 0.02     -0.15     -0.12     -0.11     -0.10
#> mu_offset[2,3]       -0.22    0.00 0.03     -0.29     -0.24     -0.21     -0.20
#> mu_offset[2,4]       -0.21    0.00 0.06     -0.37     -0.25     -0.20     -0.17
#> mu_offset[3,1]       -0.04    0.00 0.00     -0.05     -0.04     -0.04     -0.04
#> mu_offset[3,2]       -0.12    0.00 0.02     -0.15     -0.13     -0.11     -0.11
#> mu_offset[3,3]       -0.22    0.00 0.03     -0.30     -0.24     -0.22     -0.20
#> mu_offset[3,4]       -0.21    0.00 0.06     -0.37     -0.25     -0.20     -0.17
#> mu_offset[4,1]       -0.04    0.00 0.00     -0.05     -0.04     -0.04     -0.04
#> mu_offset[4,2]       -0.12    0.00 0.02     -0.15     -0.13     -0.12     -0.11
#> mu_offset[4,3]       -0.23    0.00 0.03     -0.31     -0.25     -0.23     -0.21
#> mu_offset[4,4]       -0.22    0.00 0.06     -0.37     -0.25     -0.21     -0.17
#> mu_offset[5,1]       -0.04    0.00 0.00     -0.05     -0.04     -0.04     -0.04
#> mu_offset[5,2]       -0.12    0.00 0.02     -0.15     -0.13     -0.12     -0.11
#> mu_offset[5,3]       -0.24    0.00 0.03     -0.31     -0.26     -0.23     -0.22
#> mu_offset[5,4]       -0.22    0.00 0.07     -0.37     -0.25     -0.21     -0.17
#> mu_offset[6,1]       -0.04    0.00 0.00     -0.05     -0.04     -0.04     -0.04
#> mu_offset[6,2]       -0.12    0.00 0.02     -0.16     -0.13     -0.12     -0.11
#> mu_offset[6,3]       -0.25    0.00 0.03     -0.32     -0.26     -0.24     -0.22
#> mu_offset[6,4]       -0.22    0.00 0.07     -0.38     -0.25     -0.21     -0.17
#> mu_offset[7,1]       -0.04    0.00 0.00     -0.05     -0.04     -0.04     -0.04
#> mu_offset[7,2]       -0.12    0.00 0.02     -0.16     -0.13     -0.12     -0.11
#> mu_offset[7,3]       -0.25    0.00 0.03     -0.33     -0.27     -0.25     -0.23
#> mu_offset[7,4]       -0.22    0.00 0.07     -0.38     -0.25     -0.21     -0.17
#> mu_offset[8,1]       -0.04    0.00 0.00     -0.05     -0.04     -0.04     -0.04
#> mu_offset[8,2]       -0.13    0.00 0.02     -0.16     -0.14     -0.12     -0.11
#> mu_offset[8,3]       -0.26    0.00 0.03     -0.34     -0.28     -0.26     -0.24
#> mu_offset[8,4]       -0.22    0.00 0.07     -0.38     -0.25     -0.21     -0.18
#> mu_offset[9,1]       -0.04    0.00 0.00     -0.05     -0.05     -0.04     -0.04
#> mu_offset[9,2]       -0.13    0.00 0.02     -0.16     -0.14     -0.13     -0.12
#> mu_offset[9,3]       -0.27    0.00 0.03     -0.35     -0.29     -0.26     -0.24
#> mu_offset[9,4]       -0.22    0.00 0.07     -0.38     -0.26     -0.21     -0.18
#> mu_offset[10,1]      -0.04    0.00 0.00     -0.05     -0.05     -0.04     -0.04
#> mu_offset[10,2]      -0.13    0.00 0.02     -0.17     -0.14     -0.13     -0.12
#> mu_offset[10,3]      -0.28    0.00 0.03     -0.35     -0.29     -0.27     -0.25
#> mu_offset[10,4]      -0.22    0.00 0.07     -0.38     -0.26     -0.21     -0.18
#> mu_offset[11,1]      -0.04    0.00 0.00     -0.05     -0.05     -0.04     -0.04
#> mu_offset[11,2]      -0.13    0.00 0.02     -0.17     -0.14     -0.13     -0.12
#> mu_offset[11,3]      -0.28    0.00 0.03     -0.36     -0.30     -0.28     -0.26
#> mu_offset[11,4]      -0.23    0.00 0.07     -0.38     -0.26     -0.21     -0.18
#> mu_offset[12,1]      -0.04    0.00 0.00     -0.05     -0.05     -0.04     -0.04
#> mu_offset[12,2]      -0.13    0.00 0.02     -0.17     -0.14     -0.13     -0.12
#> mu_offset[12,3]      -0.29    0.00 0.03     -0.37     -0.31     -0.29     -0.27
#> mu_offset[12,4]      -0.23    0.00 0.07     -0.38     -0.26     -0.22     -0.18
#> mu_offset[13,1]      -0.05    0.00 0.00     -0.05     -0.05     -0.05     -0.04
#> mu_offset[13,2]      -0.13    0.00 0.02     -0.17     -0.14     -0.13     -0.12
#> mu_offset[13,3]      -0.30    0.00 0.03     -0.38     -0.32     -0.29     -0.27
#> mu_offset[13,4]      -0.23    0.00 0.07     -0.39     -0.26     -0.22     -0.18
#> mu_offset[14,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.04
#> mu_offset[14,2]      -0.14    0.00 0.02     -0.17     -0.15     -0.14     -0.12
#> mu_offset[14,3]      -0.30    0.00 0.04     -0.39     -0.32     -0.30     -0.28
#> mu_offset[14,4]      -0.23    0.00 0.07     -0.39     -0.26     -0.22     -0.18
#> mu_offset[15,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.04
#> mu_offset[15,2]      -0.14    0.00 0.02     -0.18     -0.15     -0.14     -0.13
#> mu_offset[15,3]      -0.31    0.00 0.04     -0.39     -0.33     -0.31     -0.29
#> mu_offset[15,4]      -0.23    0.00 0.07     -0.39     -0.26     -0.22     -0.18
#> mu_offset[16,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.04
#> mu_offset[16,2]      -0.14    0.00 0.02     -0.18     -0.15     -0.14     -0.13
#> mu_offset[16,3]      -0.32    0.00 0.04     -0.40     -0.34     -0.31     -0.29
#> mu_offset[16,4]      -0.23    0.00 0.07     -0.39     -0.26     -0.22     -0.18
#> mu_offset[17,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.04
#> mu_offset[17,2]      -0.14    0.00 0.02     -0.18     -0.15     -0.14     -0.13
#> mu_offset[17,3]      -0.32    0.00 0.04     -0.41     -0.34     -0.32     -0.30
#> mu_offset[17,4]      -0.23    0.00 0.07     -0.39     -0.26     -0.22     -0.18
#> mu_offset[18,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.04
#> mu_offset[18,2]      -0.14    0.00 0.02     -0.18     -0.15     -0.14     -0.13
#> mu_offset[18,3]      -0.33    0.00 0.04     -0.41     -0.35     -0.32     -0.30
#> mu_offset[18,4]      -0.23    0.00 0.07     -0.39     -0.27     -0.22     -0.19
#> mu_offset[19,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.05
#> mu_offset[19,2]      -0.14    0.00 0.02     -0.18     -0.15     -0.14     -0.13
#> mu_offset[19,3]      -0.33    0.00 0.04     -0.42     -0.35     -0.33     -0.31
#> mu_offset[19,4]      -0.23    0.00 0.07     -0.39     -0.27     -0.22     -0.19
#> mu_offset[20,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.05
#> mu_offset[20,2]      -0.14    0.00 0.02     -0.18     -0.15     -0.14     -0.13
#> mu_offset[20,3]      -0.34    0.00 0.04     -0.43     -0.36     -0.33     -0.31
#> mu_offset[20,4]      -0.23    0.00 0.07     -0.39     -0.27     -0.22     -0.19
#> mu_offset[21,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.05
#> mu_offset[21,2]      -0.14    0.00 0.02     -0.18     -0.16     -0.14     -0.13
#> mu_offset[21,3]      -0.34    0.00 0.04     -0.43     -0.36     -0.34     -0.32
#> mu_offset[21,4]      -0.23    0.00 0.07     -0.40     -0.27     -0.22     -0.19
#> mu_offset[22,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.05
#> mu_offset[22,2]      -0.15    0.00 0.02     -0.18     -0.16     -0.14     -0.13
#> mu_offset[22,3]      -0.34    0.00 0.04     -0.43     -0.37     -0.34     -0.32
#> mu_offset[22,4]      -0.23    0.00 0.07     -0.40     -0.27     -0.22     -0.19
#> mu_offset[23,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.05
#> mu_offset[23,2]      -0.15    0.00 0.02     -0.18     -0.16     -0.14     -0.13
#> mu_offset[23,3]      -0.35    0.00 0.04     -0.43     -0.37     -0.34     -0.32
#> mu_offset[23,4]      -0.23    0.00 0.07     -0.40     -0.27     -0.22     -0.19
#> mu_offset[24,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.05
#> mu_offset[24,2]      -0.15    0.00 0.02     -0.19     -0.16     -0.14     -0.13
#> mu_offset[24,3]      -0.35    0.00 0.04     -0.43     -0.37     -0.34     -0.32
#> mu_offset[24,4]      -0.23    0.00 0.07     -0.40     -0.27     -0.22     -0.19
#> mu_offset[25,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.05
#> mu_offset[25,2]      -0.15    0.00 0.02     -0.19     -0.16     -0.14     -0.13
#> mu_offset[25,3]      -0.35    0.00 0.04     -0.43     -0.37     -0.34     -0.32
#> mu_offset[25,4]      -0.23    0.00 0.07     -0.40     -0.27     -0.22     -0.19
#> mu_offset[26,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.05
#> mu_offset[26,2]      -0.15    0.00 0.02     -0.18     -0.16     -0.14     -0.13
#> mu_offset[26,3]      -0.35    0.00 0.04     -0.43     -0.37     -0.34     -0.32
#> mu_offset[26,4]      -0.23    0.00 0.07     -0.40     -0.27     -0.22     -0.19
#> mu_offset[27,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.05
#> mu_offset[27,2]      -0.15    0.00 0.02     -0.18     -0.16     -0.14     -0.13
#> mu_offset[27,3]      -0.34    0.00 0.04     -0.43     -0.36     -0.34     -0.32
#> mu_offset[27,4]      -0.23    0.00 0.07     -0.40     -0.27     -0.22     -0.19
#> mu_offset[28,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.05
#> mu_offset[28,2]      -0.14    0.00 0.02     -0.18     -0.16     -0.14     -0.13
#> mu_offset[28,3]      -0.34    0.00 0.04     -0.42     -0.36     -0.33     -0.31
#> mu_offset[28,4]      -0.23    0.00 0.07     -0.40     -0.27     -0.22     -0.19
#> mu_offset[29,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.05
#> mu_offset[29,2]      -0.14    0.00 0.02     -0.18     -0.15     -0.14     -0.13
#> mu_offset[29,3]      -0.33    0.00 0.04     -0.42     -0.35     -0.33     -0.31
#> mu_offset[29,4]      -0.23    0.00 0.07     -0.39     -0.27     -0.22     -0.19
#> mu_offset[30,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.04
#> mu_offset[30,2]      -0.14    0.00 0.02     -0.18     -0.15     -0.14     -0.13
#> mu_offset[30,3]      -0.32    0.00 0.04     -0.41     -0.34     -0.32     -0.30
#> mu_offset[30,4]      -0.23    0.00 0.07     -0.39     -0.26     -0.22     -0.18
#> lp__             -78912.59    0.32 5.44 -78924.32 -78915.94 -78912.12 -78908.83
#>                      97.5% n_eff Rhat
#> beta_bs[1]           -1.58  1426 1.00
#> beta_bs[2]           -1.74   964 1.00
#> beta_bs[3]           -2.27   972 1.00
#> beta_bs[4]           -2.90   862 1.00
#> beta_bs[5]           -2.38  1483 1.00
#> sigma_layer[1]        1.48   539 1.00
#> sigma_layer[2]        1.22   255 1.01
#> lambda_raw[1]         1.09  1536 1.00
#> lambda_raw[2]         1.06   941 1.00
#> raw_phi_root[1]      -1.58  1426 1.00
#> raw_phi_root[2]      -1.64  1328 1.00
#> raw_phi_root[3]      -1.69  1133 1.00
#> raw_phi_root[4]      -1.73  1033 1.00
#> raw_phi_root[5]      -1.78  1017 1.00
#> raw_phi_root[6]      -1.83  1044 1.00
#> raw_phi_root[7]      -1.88  1097 1.00
#> raw_phi_root[8]      -1.94  1169 1.00
#> raw_phi_root[9]      -2.00  1247 1.00
#> raw_phi_root[10]     -2.06  1291 1.00
#> raw_phi_root[11]     -2.12  1314 1.00
#> raw_phi_root[12]     -2.18  1318 1.00
#> raw_phi_root[13]     -2.24  1302 1.00
#> raw_phi_root[14]     -2.30  1284 1.00
#> raw_phi_root[15]     -2.36  1270 1.00
#> raw_phi_root[16]     -2.42  1256 1.00
#> raw_phi_root[17]     -2.48  1229 1.00
#> raw_phi_root[18]     -2.54  1181 1.00
#> raw_phi_root[19]     -2.59  1119 1.00
#> raw_phi_root[20]     -2.64  1054 1.00
#> raw_phi_root[21]     -2.68   999 1.00
#> raw_phi_root[22]     -2.71   957 1.00
#> raw_phi_root[23]     -2.73   930 1.00
#> raw_phi_root[24]     -2.75   919 1.00
#> raw_phi_root[25]     -2.74   933 1.00
#> raw_phi_root[26]     -2.73   996 1.00
#> raw_phi_root[27]     -2.69  1156 1.00
#> raw_phi_root[28]     -2.62  1414 1.00
#> raw_phi_root[29]     -2.52  1545 1.00
#> raw_phi_root[30]     -2.38  1483 1.00
#> mu_offset[1,1]       -0.03  1554 1.00
#> mu_offset[1,2]       -0.09  1341 1.00
#> mu_offset[1,3]       -0.16   907 1.00
#> mu_offset[1,4]       -0.12   567 1.01
#> mu_offset[2,1]       -0.03  1556 1.00
#> mu_offset[2,2]       -0.09  1341 1.00
#> mu_offset[2,3]       -0.17   892 1.00
#> mu_offset[2,4]       -0.12   567 1.01
#> mu_offset[3,1]       -0.03  1556 1.00
#> mu_offset[3,2]       -0.09  1342 1.00
#> mu_offset[3,3]       -0.18   885 1.00
#> mu_offset[3,4]       -0.12   568 1.01
#> mu_offset[4,1]       -0.03  1558 1.00
#> mu_offset[4,2]       -0.09  1344 1.00
#> mu_offset[4,3]       -0.18   881 1.00
#> mu_offset[4,4]       -0.12   568 1.01
#> mu_offset[5,1]       -0.03  1561 1.00
#> mu_offset[5,2]       -0.09  1347 1.00
#> mu_offset[5,3]       -0.19   881 1.00
#> mu_offset[5,4]       -0.12   569 1.01
#> mu_offset[6,1]       -0.03  1565 1.00
#> mu_offset[6,2]       -0.09  1351 1.00
#> mu_offset[6,3]       -0.20   885 1.00
#> mu_offset[6,4]       -0.13   570 1.01
#> mu_offset[7,1]       -0.03  1570 1.00
#> mu_offset[7,2]       -0.10  1355 1.00
#> mu_offset[7,3]       -0.20   890 1.00
#> mu_offset[7,4]       -0.13   570 1.01
#> mu_offset[8,1]       -0.03  1574 1.00
#> mu_offset[8,2]       -0.10  1359 1.00
#> mu_offset[8,3]       -0.21   896 1.00
#> mu_offset[8,4]       -0.13   571 1.01
#> mu_offset[9,1]       -0.03  1577 1.00
#> mu_offset[9,2]       -0.10  1363 1.00
#> mu_offset[9,3]       -0.22   902 1.00
#> mu_offset[9,4]       -0.13   572 1.01
#> mu_offset[10,1]      -0.04  1579 1.00
#> mu_offset[10,2]      -0.10  1366 1.00
#> mu_offset[10,3]      -0.22   909 1.00
#> mu_offset[10,4]      -0.13   573 1.01
#> mu_offset[11,1]      -0.04  1581 1.00
#> mu_offset[11,2]      -0.10  1369 1.00
#> mu_offset[11,3]      -0.23   915 1.00
#> mu_offset[11,4]      -0.13   573 1.01
#> mu_offset[12,1]      -0.04  1581 1.00
#> mu_offset[12,2]      -0.10  1372 1.00
#> mu_offset[12,3]      -0.23   921 1.00
#> mu_offset[12,4]      -0.13   574 1.01
#> mu_offset[13,1]      -0.04  1581 1.00
#> mu_offset[13,2]      -0.11  1374 1.00
#> mu_offset[13,3]      -0.24   925 1.00
#> mu_offset[13,4]      -0.13   575 1.01
#> mu_offset[14,1]      -0.04  1580 1.00
#> mu_offset[14,2]      -0.11  1376 1.00
#> mu_offset[14,3]      -0.25   928 1.00
#> mu_offset[14,4]      -0.13   575 1.01
#> mu_offset[15,1]      -0.04  1579 1.00
#> mu_offset[15,2]      -0.11  1377 1.00
#> mu_offset[15,3]      -0.25   929 1.00
#> mu_offset[15,4]      -0.13   576 1.01
#> mu_offset[16,1]      -0.04  1578 1.00
#> mu_offset[16,2]      -0.11  1378 1.00
#> mu_offset[16,3]      -0.26   929 1.00
#> mu_offset[16,4]      -0.13   576 1.01
#> mu_offset[17,1]      -0.04  1576 1.00
#> mu_offset[17,2]      -0.11  1378 1.00
#> mu_offset[17,3]      -0.26   927 1.00
#> mu_offset[17,4]      -0.14   576 1.01
#> mu_offset[18,1]      -0.04  1574 1.00
#> mu_offset[18,2]      -0.11  1378 1.00
#> mu_offset[18,3]      -0.27   924 1.00
#> mu_offset[18,4]      -0.14   577 1.01
#> mu_offset[19,1]      -0.04  1572 1.00
#> mu_offset[19,2]      -0.11  1378 1.00
#> mu_offset[19,3]      -0.27   921 1.00
#> mu_offset[19,4]      -0.14   577 1.01
#> mu_offset[20,1]      -0.04  1570 1.00
#> mu_offset[20,2]      -0.11  1377 1.00
#> mu_offset[20,3]      -0.28   918 1.00
#> mu_offset[20,4]      -0.14   577 1.01
#> mu_offset[21,1]      -0.04  1569 1.00
#> mu_offset[21,2]      -0.11  1376 1.00
#> mu_offset[21,3]      -0.28   915 1.00
#> mu_offset[21,4]      -0.14   577 1.01
#> mu_offset[22,1]      -0.04  1568 1.00
#> mu_offset[22,2]      -0.11  1376 1.00
#> mu_offset[22,3]      -0.28   912 1.00
#> mu_offset[22,4]      -0.14   577 1.01
#> mu_offset[23,1]      -0.04  1567 1.00
#> mu_offset[23,2]      -0.11  1375 1.00
#> mu_offset[23,3]      -0.29   910 1.00
#> mu_offset[23,4]      -0.14   577 1.01
#> mu_offset[24,1]      -0.04  1567 1.00
#> mu_offset[24,2]      -0.11  1375 1.00
#> mu_offset[24,3]      -0.29   909 1.00
#> mu_offset[24,4]      -0.14   577 1.01
#> mu_offset[25,1]      -0.04  1568 1.00
#> mu_offset[25,2]      -0.11  1374 1.00
#> mu_offset[25,3]      -0.29   909 1.00
#> mu_offset[25,4]      -0.14   577 1.01
#> mu_offset[26,1]      -0.04  1570 1.00
#> mu_offset[26,2]      -0.11  1374 1.00
#> mu_offset[26,3]      -0.29   911 1.00
#> mu_offset[26,4]      -0.14   577 1.01
#> mu_offset[27,1]      -0.04  1574 1.00
#> mu_offset[27,2]      -0.11  1374 1.00
#> mu_offset[27,3]      -0.28   915 1.00
#> mu_offset[27,4]      -0.14   577 1.01
#> mu_offset[28,1]      -0.04  1580 1.00
#> mu_offset[28,2]      -0.11  1374 1.00
#> mu_offset[28,3]      -0.28   923 1.00
#> mu_offset[28,4]      -0.14   577 1.01
#> mu_offset[29,1]      -0.04  1588 1.00
#> mu_offset[29,2]      -0.11  1375 1.00
#> mu_offset[29,3]      -0.27   937 1.00
#> mu_offset[29,4]      -0.14   577 1.01
#> mu_offset[30,1]      -0.04  1599 1.00
#> mu_offset[30,2]      -0.11  1377 1.00
#> mu_offset[30,3]      -0.26   960 1.00
#> mu_offset[30,4]      -0.14   577 1.01
#> lp__             -78903.07   286 1.01
#> 
#> Samples were drawn using NUTS(diag_e) at Wed Nov  4 17:28:26 2026.
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
#>                      State 0.8878245 0.8856615 0.8878544 0.8902848
#>                    Scruggs 0.8939658 0.8912540 0.8939906 0.8968009
#>                     Simone 0.8601002 0.8555973 0.8601033 0.8646928
#>                     Watson 0.9060555 0.9027639 0.9057943 0.9094405
#>       Chickadee Elementary 0.9133784 0.9079804 0.9131943 0.9193967
#>           Nuthatch Academy 0.9027407 0.8987344 0.9030652 0.9062080
#>          Blue Heron School 0.9126263 0.9065211 0.9125707 0.9186671
#>      Flycatcher Elementary 0.9277678 0.9205247 0.9274894 0.9343778
#>   Bluebird Learning Center 0.9242663 0.9147597 0.9245083 0.9330114
#>            Catbird Academy 0.8907372 0.8868976 0.8907955 0.8951920
#>           Finch Elementary 0.8969255 0.8855923 0.8970739 0.9083713
#>             Sparrow School 0.9202639 0.9135316 0.9202074 0.9277598
#>  Towhee Children's Academy 0.8393362 0.8328179 0.8394243 0.8447683
#>         Warbler Elementary 0.8550253 0.8402931 0.8550407 0.8696575
#>           Egret Elementary 0.9187425 0.9139060 0.9185555 0.9240307
#>           Cardinal Academy 0.6804674 0.6401672 0.6798433 0.7108478
#>             Bunting School 0.7786984 0.7654491 0.7784047 0.7920114
#>            Tanager Academy 0.9323010 0.9271487 0.9321082 0.9390435
#>       Oriole Youth Academy 0.8626127 0.8539776 0.8628151 0.8718147
#>   Grosbeak Learning Center 0.6628541 0.6305595 0.6643707 0.6893751
#>           Junco Elementary 0.8840079 0.8783874 0.8838529 0.8902746
#>          Meadowlark School 0.8871184 0.8764476 0.8874724 0.8956858
#>       Goldfinch Elementary 0.9079878 0.9014083 0.9080572 0.9142060
#>        Mockingbird Academy 0.8901826 0.8797083 0.8905847 0.8996427
#>    Kinglet Learning Center 0.9315590 0.9271238 0.9315186 0.9360863
#>               Vireo School 0.9329628 0.9257458 0.9329710 0.9388942
#>         Kingfisher Academy 0.8823459 0.8706327 0.8829234 0.8921433
#>       Cormorant Elementary 0.9040213 0.8981008 0.9042763 0.9090619
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
