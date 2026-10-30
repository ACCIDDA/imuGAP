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
#> beta_bs[2]           -1.84    0.00 0.05     -1.94     -1.88     -1.84     -1.80
#> beta_bs[3]           -2.44    0.00 0.08     -2.60     -2.50     -2.44     -2.38
#> beta_bs[4]           -3.10    0.00 0.09     -3.28     -3.16     -3.10     -3.03
#> beta_bs[5]           -2.56    0.00 0.09     -2.73     -2.62     -2.56     -2.50
#> sigma_layer[1]        0.64    0.02 0.37      0.22      0.39      0.54      0.79
#> sigma_layer[2]        0.82    0.01 0.15      0.59      0.72      0.80      0.91
#> lambda_raw[1]         1.04    0.00 0.03      0.99      1.02      1.04      1.06
#> lambda_raw[2]         1.03    0.00 0.01      1.01      1.02      1.03      1.04
#> raw_phi_root[1]      -1.64    0.00 0.03     -1.70     -1.66     -1.64     -1.62
#> raw_phi_root[2]      -1.68    0.00 0.02     -1.73     -1.70     -1.69     -1.67
#> raw_phi_root[3]      -1.73    0.00 0.02     -1.77     -1.74     -1.73     -1.72
#> raw_phi_root[4]      -1.78    0.00 0.02     -1.82     -1.79     -1.78     -1.76
#> raw_phi_root[5]      -1.83    0.00 0.02     -1.87     -1.84     -1.83     -1.81
#> raw_phi_root[6]      -1.88    0.00 0.02     -1.93     -1.90     -1.88     -1.86
#> raw_phi_root[7]      -1.93    0.00 0.02     -1.98     -1.95     -1.93     -1.92
#> raw_phi_root[8]      -1.99    0.00 0.02     -2.04     -2.01     -1.99     -1.97
#> raw_phi_root[9]      -2.05    0.00 0.02     -2.10     -2.07     -2.05     -2.03
#> raw_phi_root[10]     -2.11    0.00 0.03     -2.16     -2.13     -2.11     -2.09
#> raw_phi_root[11]     -2.17    0.00 0.03     -2.22     -2.19     -2.17     -2.15
#> raw_phi_root[12]     -2.23    0.00 0.03     -2.28     -2.25     -2.23     -2.21
#> raw_phi_root[13]     -2.29    0.00 0.03     -2.35     -2.31     -2.29     -2.27
#> raw_phi_root[14]     -2.36    0.00 0.03     -2.42     -2.38     -2.36     -2.34
#> raw_phi_root[15]     -2.42    0.00 0.03     -2.49     -2.44     -2.42     -2.40
#> raw_phi_root[16]     -2.49    0.00 0.03     -2.55     -2.51     -2.49     -2.46
#> raw_phi_root[17]     -2.55    0.00 0.04     -2.62     -2.58     -2.55     -2.53
#> raw_phi_root[18]     -2.61    0.00 0.04     -2.69     -2.64     -2.61     -2.59
#> raw_phi_root[19]     -2.67    0.00 0.04     -2.75     -2.70     -2.67     -2.65
#> raw_phi_root[20]     -2.72    0.00 0.04     -2.81     -2.75     -2.72     -2.70
#> raw_phi_root[21]     -2.77    0.00 0.04     -2.86     -2.80     -2.77     -2.74
#> raw_phi_root[22]     -2.81    0.00 0.05     -2.90     -2.84     -2.80     -2.77
#> raw_phi_root[23]     -2.83    0.00 0.05     -2.93     -2.86     -2.83     -2.80
#> raw_phi_root[24]     -2.84    0.00 0.05     -2.94     -2.88     -2.84     -2.81
#> raw_phi_root[25]     -2.84    0.00 0.05     -2.94     -2.88     -2.84     -2.81
#> raw_phi_root[26]     -2.82    0.00 0.05     -2.92     -2.86     -2.82     -2.79
#> raw_phi_root[27]     -2.79    0.00 0.05     -2.88     -2.82     -2.79     -2.76
#> raw_phi_root[28]     -2.73    0.00 0.05     -2.84     -2.77     -2.73     -2.70
#> raw_phi_root[29]     -2.66    0.00 0.07     -2.79     -2.70     -2.65     -2.61
#> raw_phi_root[30]     -2.56    0.00 0.09     -2.73     -2.62     -2.56     -2.50
#> mu_offset[1,1]       -0.04    0.00 0.00     -0.04     -0.04     -0.04     -0.03
#> mu_offset[1,2]       -0.11    0.00 0.01     -0.14     -0.12     -0.11     -0.10
#> mu_offset[1,3]       -0.21    0.00 0.03     -0.29     -0.23     -0.21     -0.19
#> mu_offset[1,4]       -0.21    0.00 0.06     -0.37     -0.25     -0.20     -0.17
#> mu_offset[2,1]       -0.04    0.00 0.00     -0.05     -0.04     -0.04     -0.03
#> mu_offset[2,2]       -0.11    0.00 0.01     -0.15     -0.12     -0.11     -0.10
#> mu_offset[2,3]       -0.22    0.00 0.03     -0.30     -0.23     -0.22     -0.20
#> mu_offset[2,4]       -0.21    0.00 0.06     -0.38     -0.25     -0.20     -0.17
#> mu_offset[3,1]       -0.04    0.00 0.00     -0.05     -0.04     -0.04     -0.04
#> mu_offset[3,2]       -0.12    0.00 0.02     -0.15     -0.13     -0.12     -0.11
#> mu_offset[3,3]       -0.23    0.00 0.03     -0.30     -0.24     -0.22     -0.21
#> mu_offset[3,4]       -0.22    0.00 0.06     -0.38     -0.25     -0.21     -0.17
#> mu_offset[4,1]       -0.04    0.00 0.00     -0.05     -0.04     -0.04     -0.04
#> mu_offset[4,2]       -0.12    0.00 0.02     -0.15     -0.13     -0.12     -0.11
#> mu_offset[4,3]       -0.23    0.00 0.03     -0.31     -0.25     -0.23     -0.21
#> mu_offset[4,4]       -0.22    0.00 0.06     -0.38     -0.25     -0.21     -0.17
#> mu_offset[5,1]       -0.04    0.00 0.00     -0.05     -0.04     -0.04     -0.04
#> mu_offset[5,2]       -0.12    0.00 0.02     -0.15     -0.13     -0.12     -0.11
#> mu_offset[5,3]       -0.24    0.00 0.03     -0.32     -0.26     -0.24     -0.22
#> mu_offset[5,4]       -0.22    0.00 0.06     -0.38     -0.25     -0.21     -0.17
#> mu_offset[6,1]       -0.04    0.00 0.00     -0.05     -0.04     -0.04     -0.04
#> mu_offset[6,2]       -0.12    0.00 0.02     -0.16     -0.13     -0.12     -0.11
#> mu_offset[6,3]       -0.25    0.00 0.03     -0.33     -0.26     -0.24     -0.23
#> mu_offset[6,4]       -0.22    0.00 0.06     -0.38     -0.25     -0.21     -0.17
#> mu_offset[7,1]       -0.04    0.00 0.00     -0.05     -0.04     -0.04     -0.04
#> mu_offset[7,2]       -0.12    0.00 0.02     -0.16     -0.13     -0.12     -0.11
#> mu_offset[7,3]       -0.26    0.00 0.03     -0.33     -0.27     -0.25     -0.23
#> mu_offset[7,4]       -0.22    0.00 0.06     -0.38     -0.25     -0.21     -0.18
#> mu_offset[8,1]       -0.04    0.00 0.00     -0.05     -0.04     -0.04     -0.04
#> mu_offset[8,2]       -0.13    0.00 0.02     -0.16     -0.14     -0.12     -0.12
#> mu_offset[8,3]       -0.26    0.00 0.03     -0.34     -0.28     -0.26     -0.24
#> mu_offset[8,4]       -0.22    0.00 0.06     -0.39     -0.26     -0.21     -0.18
#> mu_offset[9,1]       -0.04    0.00 0.00     -0.05     -0.05     -0.04     -0.04
#> mu_offset[9,2]       -0.13    0.00 0.02     -0.16     -0.14     -0.13     -0.12
#> mu_offset[9,3]       -0.27    0.00 0.03     -0.35     -0.29     -0.27     -0.25
#> mu_offset[9,4]       -0.22    0.00 0.06     -0.39     -0.26     -0.21     -0.18
#> mu_offset[10,1]      -0.04    0.00 0.00     -0.05     -0.05     -0.04     -0.04
#> mu_offset[10,2]      -0.13    0.00 0.02     -0.16     -0.14     -0.13     -0.12
#> mu_offset[10,3]      -0.28    0.00 0.03     -0.36     -0.30     -0.27     -0.25
#> mu_offset[10,4]      -0.22    0.00 0.06     -0.39     -0.26     -0.21     -0.18
#> mu_offset[11,1]      -0.04    0.00 0.00     -0.05     -0.05     -0.04     -0.04
#> mu_offset[11,2]      -0.13    0.00 0.02     -0.17     -0.14     -0.13     -0.12
#> mu_offset[11,3]      -0.28    0.00 0.03     -0.36     -0.30     -0.28     -0.26
#> mu_offset[11,4]      -0.23    0.00 0.06     -0.39     -0.26     -0.22     -0.18
#> mu_offset[12,1]      -0.04    0.00 0.00     -0.05     -0.05     -0.04     -0.04
#> mu_offset[12,2]      -0.13    0.00 0.02     -0.17     -0.14     -0.13     -0.12
#> mu_offset[12,3]      -0.29    0.00 0.03     -0.37     -0.31     -0.29     -0.27
#> mu_offset[12,4]      -0.23    0.00 0.06     -0.39     -0.26     -0.22     -0.18
#> mu_offset[13,1]      -0.05    0.00 0.00     -0.05     -0.05     -0.05     -0.04
#> mu_offset[13,2]      -0.13    0.00 0.02     -0.17     -0.15     -0.13     -0.12
#> mu_offset[13,3]      -0.30    0.00 0.03     -0.38     -0.32     -0.29     -0.28
#> mu_offset[13,4]      -0.23    0.00 0.06     -0.39     -0.26     -0.22     -0.18
#> mu_offset[14,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.04
#> mu_offset[14,2]      -0.14    0.00 0.02     -0.17     -0.15     -0.14     -0.12
#> mu_offset[14,3]      -0.31    0.00 0.03     -0.39     -0.32     -0.30     -0.28
#> mu_offset[14,4]      -0.23    0.00 0.07     -0.40     -0.26     -0.22     -0.18
#> mu_offset[15,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.04
#> mu_offset[15,2]      -0.14    0.00 0.02     -0.17     -0.15     -0.14     -0.13
#> mu_offset[15,3]      -0.31    0.00 0.04     -0.39     -0.33     -0.31     -0.29
#> mu_offset[15,4]      -0.23    0.00 0.07     -0.40     -0.26     -0.22     -0.18
#> mu_offset[16,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.04
#> mu_offset[16,2]      -0.14    0.00 0.02     -0.18     -0.15     -0.14     -0.13
#> mu_offset[16,3]      -0.32    0.00 0.04     -0.40     -0.34     -0.31     -0.29
#> mu_offset[16,4]      -0.23    0.00 0.07     -0.40     -0.27     -0.22     -0.19
#> mu_offset[17,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.04
#> mu_offset[17,2]      -0.14    0.00 0.02     -0.18     -0.15     -0.14     -0.13
#> mu_offset[17,3]      -0.32    0.00 0.04     -0.41     -0.34     -0.32     -0.30
#> mu_offset[17,4]      -0.23    0.00 0.07     -0.40     -0.27     -0.22     -0.19
#> mu_offset[18,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.04
#> mu_offset[18,2]      -0.14    0.00 0.02     -0.18     -0.15     -0.14     -0.13
#> mu_offset[18,3]      -0.33    0.00 0.04     -0.41     -0.35     -0.33     -0.31
#> mu_offset[18,4]      -0.23    0.00 0.07     -0.40     -0.27     -0.22     -0.19
#> mu_offset[19,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.05
#> mu_offset[19,2]      -0.14    0.00 0.02     -0.18     -0.15     -0.14     -0.13
#> mu_offset[19,3]      -0.34    0.00 0.04     -0.42     -0.35     -0.33     -0.31
#> mu_offset[19,4]      -0.23    0.00 0.07     -0.40     -0.27     -0.22     -0.19
#> mu_offset[20,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.05
#> mu_offset[20,2]      -0.14    0.00 0.02     -0.18     -0.16     -0.14     -0.13
#> mu_offset[20,3]      -0.34    0.00 0.04     -0.42     -0.36     -0.33     -0.32
#> mu_offset[20,4]      -0.23    0.00 0.07     -0.40     -0.27     -0.22     -0.19
#> mu_offset[21,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.05
#> mu_offset[21,2]      -0.14    0.00 0.02     -0.18     -0.16     -0.14     -0.13
#> mu_offset[21,3]      -0.34    0.00 0.04     -0.43     -0.36     -0.34     -0.32
#> mu_offset[21,4]      -0.23    0.00 0.07     -0.40     -0.27     -0.22     -0.19
#> mu_offset[22,1]      -0.05    0.00 0.01     -0.06     -0.05     -0.05     -0.05
#> mu_offset[22,2]      -0.15    0.00 0.02     -0.18     -0.16     -0.14     -0.13
#> mu_offset[22,3]      -0.35    0.00 0.04     -0.43     -0.37     -0.34     -0.32
#> mu_offset[22,4]      -0.23    0.00 0.07     -0.40     -0.27     -0.22     -0.19
#> mu_offset[23,1]      -0.05    0.00 0.01     -0.06     -0.05     -0.05     -0.05
#> mu_offset[23,2]      -0.15    0.00 0.02     -0.18     -0.16     -0.14     -0.13
#> mu_offset[23,3]      -0.35    0.00 0.04     -0.43     -0.37     -0.34     -0.32
#> mu_offset[23,4]      -0.23    0.00 0.07     -0.40     -0.27     -0.22     -0.19
#> mu_offset[24,1]      -0.05    0.00 0.01     -0.06     -0.05     -0.05     -0.05
#> mu_offset[24,2]      -0.15    0.00 0.02     -0.18     -0.16     -0.15     -0.13
#> mu_offset[24,3]      -0.35    0.00 0.04     -0.43     -0.37     -0.34     -0.32
#> mu_offset[24,4]      -0.24    0.00 0.07     -0.40     -0.27     -0.22     -0.19
#> mu_offset[25,1]      -0.05    0.00 0.01     -0.06     -0.05     -0.05     -0.05
#> mu_offset[25,2]      -0.15    0.00 0.02     -0.18     -0.16     -0.15     -0.13
#> mu_offset[25,3]      -0.35    0.00 0.04     -0.43     -0.37     -0.34     -0.32
#> mu_offset[25,4]      -0.24    0.00 0.07     -0.40     -0.27     -0.22     -0.19
#> mu_offset[26,1]      -0.05    0.00 0.01     -0.06     -0.05     -0.05     -0.05
#> mu_offset[26,2]      -0.15    0.00 0.02     -0.18     -0.16     -0.14     -0.13
#> mu_offset[26,3]      -0.35    0.00 0.04     -0.43     -0.37     -0.34     -0.32
#> mu_offset[26,4]      -0.23    0.00 0.07     -0.40     -0.27     -0.22     -0.19
#> mu_offset[27,1]      -0.05    0.00 0.01     -0.06     -0.05     -0.05     -0.05
#> mu_offset[27,2]      -0.15    0.00 0.02     -0.18     -0.16     -0.14     -0.13
#> mu_offset[27,3]      -0.34    0.00 0.04     -0.43     -0.36     -0.34     -0.32
#> mu_offset[27,4]      -0.23    0.00 0.07     -0.40     -0.27     -0.22     -0.19
#> mu_offset[28,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.05
#> mu_offset[28,2]      -0.14    0.00 0.02     -0.18     -0.16     -0.14     -0.13
#> mu_offset[28,3]      -0.34    0.00 0.04     -0.42     -0.36     -0.34     -0.32
#> mu_offset[28,4]      -0.23    0.00 0.07     -0.40     -0.27     -0.22     -0.19
#> mu_offset[29,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.05
#> mu_offset[29,2]      -0.14    0.00 0.02     -0.18     -0.15     -0.14     -0.13
#> mu_offset[29,3]      -0.33    0.00 0.04     -0.42     -0.35     -0.33     -0.31
#> mu_offset[29,4]      -0.23    0.00 0.07     -0.40     -0.27     -0.22     -0.19
#> mu_offset[30,1]      -0.05    0.00 0.00     -0.06     -0.05     -0.05     -0.04
#> mu_offset[30,2]      -0.14    0.00 0.02     -0.18     -0.15     -0.14     -0.13
#> mu_offset[30,3]      -0.33    0.00 0.04     -0.41     -0.35     -0.32     -0.30
#> mu_offset[30,4]      -0.23    0.00 0.07     -0.40     -0.27     -0.22     -0.19
#> lp__             -78913.00    0.28 5.07 -78923.92 -78916.23 -78912.77 -78909.41
#>                      97.5% n_eff Rhat
#> beta_bs[1]           -1.58  1366 1.00
#> beta_bs[2]           -1.75  1257 1.00
#> beta_bs[3]           -2.28  1060 1.00
#> beta_bs[4]           -2.91  1095 1.00
#> beta_bs[5]           -2.40  1217 1.00
#> sigma_layer[1]        1.68   236 1.01
#> sigma_layer[2]        1.18   183 1.03
#> lambda_raw[1]         1.09  1272 1.00
#> lambda_raw[2]         1.06  1046 1.00
#> raw_phi_root[1]      -1.58  1366 1.00
#> raw_phi_root[2]      -1.64  1370 1.00
#> raw_phi_root[3]      -1.69  1313 1.00
#> raw_phi_root[4]      -1.73  1255 1.00
#> raw_phi_root[5]      -1.78  1247 1.00
#> raw_phi_root[6]      -1.83  1262 1.00
#> raw_phi_root[7]      -1.89  1282 1.00
#> raw_phi_root[8]      -1.94  1294 1.00
#> raw_phi_root[9]      -2.00  1276 1.00
#> raw_phi_root[10]     -2.06  1232 1.00
#> raw_phi_root[11]     -2.12  1179 1.00
#> raw_phi_root[12]     -2.18  1133 1.00
#> raw_phi_root[13]     -2.24  1101 1.00
#> raw_phi_root[14]     -2.30  1083 1.00
#> raw_phi_root[15]     -2.36  1080 1.00
#> raw_phi_root[16]     -2.42  1088 1.00
#> raw_phi_root[17]     -2.48  1105 1.00
#> raw_phi_root[18]     -2.54  1123 1.00
#> raw_phi_root[19]     -2.59  1118 1.00
#> raw_phi_root[20]     -2.64  1135 1.00
#> raw_phi_root[21]     -2.68  1120 1.00
#> raw_phi_root[22]     -2.71  1103 1.00
#> raw_phi_root[23]     -2.73  1088 1.00
#> raw_phi_root[24]     -2.75  1077 1.00
#> raw_phi_root[25]     -2.75  1077 1.00
#> raw_phi_root[26]     -2.73  1080 1.00
#> raw_phi_root[27]     -2.70  1102 1.00
#> raw_phi_root[28]     -2.63  1152 1.00
#> raw_phi_root[29]     -2.53  1203 1.00
#> raw_phi_root[30]     -2.40  1217 1.00
#> mu_offset[1,1]       -0.03  1809 1.00
#> mu_offset[1,2]       -0.09  1336 1.00
#> mu_offset[1,3]       -0.17   933 1.00
#> mu_offset[1,4]       -0.12   605 1.01
#> mu_offset[2,1]       -0.03  1814 1.00
#> mu_offset[2,2]       -0.09  1333 1.00
#> mu_offset[2,3]       -0.17   944 1.00
#> mu_offset[2,4]       -0.12   606 1.01
#> mu_offset[3,1]       -0.03  1816 1.00
#> mu_offset[3,2]       -0.09  1330 1.00
#> mu_offset[3,3]       -0.18   952 1.00
#> mu_offset[3,4]       -0.12   607 1.01
#> mu_offset[4,1]       -0.03  1816 1.00
#> mu_offset[4,2]       -0.09  1328 1.00
#> mu_offset[4,3]       -0.18   959 1.00
#> mu_offset[4,4]       -0.12   608 1.01
#> mu_offset[5,1]       -0.03  1817 1.00
#> mu_offset[5,2]       -0.09  1327 1.00
#> mu_offset[5,3]       -0.19   963 1.00
#> mu_offset[5,4]       -0.12   608 1.01
#> mu_offset[6,1]       -0.03  1819 1.00
#> mu_offset[6,2]       -0.09  1326 1.00
#> mu_offset[6,3]       -0.20   965 1.00
#> mu_offset[6,4]       -0.13   609 1.01
#> mu_offset[7,1]       -0.03  1821 1.00
#> mu_offset[7,2]       -0.10  1325 1.00
#> mu_offset[7,3]       -0.20   966 1.00
#> mu_offset[7,4]       -0.13   609 1.01
#> mu_offset[8,1]       -0.03  1823 1.00
#> mu_offset[8,2]       -0.10  1325 1.00
#> mu_offset[8,3]       -0.21   966 1.00
#> mu_offset[8,4]       -0.13   609 1.01
#> mu_offset[9,1]       -0.03  1824 1.00
#> mu_offset[9,2]       -0.10  1300 1.00
#> mu_offset[9,3]       -0.22   966 1.00
#> mu_offset[9,4]       -0.13   609 1.01
#> mu_offset[10,1]      -0.03  1825 1.00
#> mu_offset[10,2]      -0.10  1282 1.00
#> mu_offset[10,3]      -0.23   969 1.00
#> mu_offset[10,4]      -0.13   610 1.01
#> mu_offset[11,1]      -0.04  1825 1.00
#> mu_offset[11,2]      -0.10  1267 1.00
#> mu_offset[11,3]      -0.23   972 1.00
#> mu_offset[11,4]      -0.13   610 1.01
#> mu_offset[12,1]      -0.04  1824 1.00
#> mu_offset[12,2]      -0.10  1253 1.00
#> mu_offset[12,3]      -0.24   975 1.00
#> mu_offset[12,4]      -0.13   610 1.01
#> mu_offset[13,1]      -0.04  1823 1.00
#> mu_offset[13,2]      -0.11  1240 1.00
#> mu_offset[13,3]      -0.24   978 1.00
#> mu_offset[13,4]      -0.13   611 1.01
#> mu_offset[14,1]      -0.04  1822 1.00
#> mu_offset[14,2]      -0.11  1228 1.00
#> mu_offset[14,3]      -0.25   983 1.00
#> mu_offset[14,4]      -0.13   611 1.01
#> mu_offset[15,1]      -0.04  1821 1.00
#> mu_offset[15,2]      -0.11  1219 1.00
#> mu_offset[15,3]      -0.26   987 1.00
#> mu_offset[15,4]      -0.13   611 1.01
#> mu_offset[16,1]      -0.04  1819 1.00
#> mu_offset[16,2]      -0.11  1210 1.00
#> mu_offset[16,3]      -0.26   993 1.00
#> mu_offset[16,4]      -0.13   611 1.01
#> mu_offset[17,1]      -0.04  1817 1.00
#> mu_offset[17,2]      -0.11  1208 1.00
#> mu_offset[17,3]      -0.27   999 1.00
#> mu_offset[17,4]      -0.14   612 1.01
#> mu_offset[18,1]      -0.04  1815 1.00
#> mu_offset[18,2]      -0.11  1210 1.00
#> mu_offset[18,3]      -0.27  1005 1.00
#> mu_offset[18,4]      -0.14   612 1.01
#> mu_offset[19,1]      -0.04  1813 1.00
#> mu_offset[19,2]      -0.11  1213 1.00
#> mu_offset[19,3]      -0.28  1011 1.00
#> mu_offset[19,4]      -0.14   613 1.01
#> mu_offset[20,1]      -0.04  1811 1.00
#> mu_offset[20,2]      -0.11  1216 1.00
#> mu_offset[20,3]      -0.28  1016 1.00
#> mu_offset[20,4]      -0.14   613 1.01
#> mu_offset[21,1]      -0.04  1809 1.00
#> mu_offset[21,2]      -0.11  1215 1.00
#> mu_offset[21,3]      -0.28  1022 1.00
#> mu_offset[21,4]      -0.14   613 1.01
#> mu_offset[22,1]      -0.04  1807 1.00
#> mu_offset[22,2]      -0.11  1214 1.00
#> mu_offset[22,3]      -0.29  1026 1.00
#> mu_offset[22,4]      -0.14   613 1.01
#> mu_offset[23,1]      -0.04  1805 1.00
#> mu_offset[23,2]      -0.12  1213 1.00
#> mu_offset[23,3]      -0.29  1031 1.00
#> mu_offset[23,4]      -0.14   613 1.01
#> mu_offset[24,1]      -0.04  1804 1.00
#> mu_offset[24,2]      -0.12  1212 1.00
#> mu_offset[24,3]      -0.29  1034 1.00
#> mu_offset[24,4]      -0.14   613 1.01
#> mu_offset[25,1]      -0.04  1803 1.00
#> mu_offset[25,2]      -0.12  1212 1.00
#> mu_offset[25,3]      -0.29  1037 1.00
#> mu_offset[25,4]      -0.14   614 1.01
#> mu_offset[26,1]      -0.04  1802 1.00
#> mu_offset[26,2]      -0.12  1210 1.00
#> mu_offset[26,3]      -0.29  1039 1.00
#> mu_offset[26,4]      -0.14   613 1.01
#> mu_offset[27,1]      -0.04  1801 1.00
#> mu_offset[27,2]      -0.11  1212 1.00
#> mu_offset[27,3]      -0.29  1041 1.00
#> mu_offset[27,4]      -0.14   613 1.01
#> mu_offset[28,1]      -0.04  1799 1.00
#> mu_offset[28,2]      -0.11  1209 1.00
#> mu_offset[28,3]      -0.28  1043 1.00
#> mu_offset[28,4]      -0.14   613 1.01
#> mu_offset[29,1]      -0.04  1796 1.00
#> mu_offset[29,2]      -0.11  1205 1.00
#> mu_offset[29,3]      -0.27  1045 1.00
#> mu_offset[29,4]      -0.14   612 1.01
#> mu_offset[30,1]      -0.04  1793 1.00
#> mu_offset[30,2]      -0.11  1199 1.00
#> mu_offset[30,3]      -0.26  1048 1.00
#> mu_offset[30,4]      -0.13   612 1.01
#> lp__             -78904.11   335 1.02
#> 
#> Samples were drawn using NUTS(diag_e) at Fri Oct 30 19:31:58 2026.
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
#>                      State 0.8878032 0.8852987 0.8875562 0.8906698
#>                    Scruggs 0.8937628 0.8909959 0.8937161 0.8963253
#>                     Simone 0.8600651 0.8559270 0.8599518 0.8643935
#>                     Watson 0.9064980 0.9032769 0.9065105 0.9091944
#>       Chickadee Elementary 0.9131604 0.9068028 0.9130856 0.9187215
#>           Nuthatch Academy 0.9024620 0.8984313 0.9027665 0.9058210
#>          Blue Heron School 0.9125660 0.9061342 0.9127316 0.9178757
#>      Flycatcher Elementary 0.9278484 0.9208339 0.9278294 0.9338910
#>   Bluebird Learning Center 0.9241835 0.9176117 0.9237295 0.9311364
#>            Catbird Academy 0.8906144 0.8866722 0.8906587 0.8941498
#>           Finch Elementary 0.8963324 0.8850702 0.8959203 0.9062838
#>             Sparrow School 0.9199495 0.9133897 0.9198706 0.9277231
#>  Towhee Children's Academy 0.8389786 0.8295600 0.8391426 0.8456567
#>         Warbler Elementary 0.8551229 0.8412032 0.8551134 0.8678965
#>           Egret Elementary 0.9187050 0.9129856 0.9186834 0.9228931
#>           Cardinal Academy 0.6786802 0.6342083 0.6776151 0.7162135
#>             Bunting School 0.7787742 0.7636460 0.7773413 0.7931666
#>            Tanager Academy 0.9325040 0.9231881 0.9325381 0.9391371
#>       Oriole Youth Academy 0.8629095 0.8560042 0.8633555 0.8700749
#>   Grosbeak Learning Center 0.6602598 0.6198290 0.6604024 0.6933443
#>           Junco Elementary 0.8841745 0.8781567 0.8840094 0.8907699
#>          Meadowlark School 0.8872046 0.8742203 0.8870553 0.8984135
#>       Goldfinch Elementary 0.9082986 0.9016996 0.9082867 0.9135369
#>        Mockingbird Academy 0.8906026 0.8819278 0.8900154 0.9015363
#>    Kinglet Learning Center 0.9322793 0.9267635 0.9322459 0.9373784
#>               Vireo School 0.9332011 0.9270000 0.9334777 0.9384134
#>         Kingfisher Academy 0.8828989 0.8710538 0.8829818 0.8928994
#>       Cormorant Elementary 0.9045580 0.8986728 0.9045236 0.9100749
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
