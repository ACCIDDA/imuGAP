# Fit Inspection and Stan Diagnostics

## Introduction

Because `imuGAP` estimates an underlying process model for vaccination
(including baseline uptake splines $`\beta_{\text{bs}}`$,
force-of-vaccination rates $`\lambda`$, and hierarchical location
offsets $`\delta`$) via Bayesian inference in Stan, evaluating a model
fit entails assessing whether MCMC chains converged without pathologies.
Ideally, you would also synthesize data in a way you think is
representative of your system and attempt to recover the latent process
parameters. The example data in the package demonstrates that workflow.

This vignette demonstrates how to:

1.  Load and extract posterior parameter draws from an `imugap_fit`
    object using
    [`extract_imugap()`](https://accidda.github.io/imuGAP/reference/extract_imugap.md).
2.  Inspect parameter trace plots across multiple chains.
3.  Compare posterior parameter estimates against true data-generating
    simulation values (`latent_params_sim`).
4.  Evaluate spatial variance components ($`\sigma`$) and
    location-specific random offsets ($`\delta`$).

------------------------------------------------------------------------

## 1. Loading the Model Fit and Extracting Parameters

You can load the bundled `fit_sim` object, which wraps the underlying
Stan model fit along with dataset and options metadata:

``` r

data("fit_sim", package = "imuGAP")
data("latent_params_sim", package = "imuGAP")
data("locations_sim", package = "imuGAP")
data("observations_sim", package = "imuGAP")

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

To extract posterior draws for specific model parameters, use
[`extract_imugap()`](https://accidda.github.io/imuGAP/reference/extract_imugap.md):

``` r

# Extract B-spline basis coefficients
beta_draws <- extract_imugap(fit_sim, pars = "beta_bs")
str(beta_draws)
#> List of 1
#>  $ beta_bs: num [1:500, 1:5] -1.62 -1.66 -1.57 -1.63 -1.66 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ iterations: NULL
#>   .. ..$           : NULL

# Extract unconstrained force of vaccination rate parameters
lambda_draws <- extract_imugap(fit_sim, pars = "lambda_raw")
str(lambda_draws)
#> List of 1
#>  $ lambda_raw: num [1:500, 1:2] 1.04 1.06 1.03 1.03 1.01 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ iterations: NULL
#>   .. ..$           : NULL
```

------------------------------------------------------------------------

## 2. Visual Diagnostics with `bayesplot` and Parameter Recovery

#### State-Level Cohort Uptake: Basis Spline Coefficients ($`\beta_{\text{bs}}`$)

The state-level baseline propensity curve is parameterized as a B-spline
over birth cohorts with coefficients $`\beta_{\text{bs}}`$. The
following trace plots show parameter draws across all 4 MCMC chains
alongside within-chain medians and the true simulation parameters
(dashed red lines):

**Show plot code**

``` r

beta_pars <- paste0("beta_bs[", seq_along(latent_params_sim$beta_bs), "]")
beta_arr <- as.array(fit_sim$raw_fit, pars = beta_pars)
beta_chain_meds <- rbindlist(lapply(beta_pars, function(p) {
  data.table(
    parameter = p,
    Chain = factor(seq_len(dim(beta_arr)[2])),
    med_val = apply(beta_arr[, , p, drop = FALSE], 2, median)
  )
}))

beta_ref <- data.frame(
  parameter = beta_pars,
  true_val = latent_params_sim$beta_bs,
  label = sprintf(
    "True~beta[%d] == %.2f",
    seq_along(latent_params_sim$beta_bs),
    latent_params_sim$beta_bs
  )
)

bayesplot::mcmc_trace(
  beta_arr,
  facet_args = list(labeller = ggplot2::as_labeller(function(x) {
    gsub("beta_bs\\[(\\d+)\\]", "beta[\\1]", x)
  }, default = ggplot2::label_parsed))
) +
  geom_hline(
    data = beta_chain_meds,
    aes(yintercept = med_val, color = Chain),
    linetype = "solid", linewidth = 0.5, alpha = 0.8
  ) +
  geom_hline(
    data = beta_ref,
    aes(yintercept = true_val),
    color = "firebrick", linetype = "dashed", linewidth = 0.8
  ) +
  geom_label(
    data = beta_ref,
    aes(x = 100, y = true_val, label = label),
    parse = TRUE, color = "firebrick", fill = ggplot2::alpha("white", 0.75),
    linewidth = NA, vjust = -0.3, hjust = 0, size = 3.2
  ) +
  theme(legend.position = "none")
```

![](examining_fits_files/figure-html/trace-plot-beta-1.png)

------------------------------------------------------------------------

#### Hierarchy Layer Variances ($`\sigma_{\text{layer}}`$)

In multi-layer models, `sigma_layer` captures the standard deviation of
location random offsets at each level of the tree
(e.g. $`\sigma_{\text{county}}`$ at layer 1 and
$`\sigma_{\text{school}}`$ at layer 2):

**Show plot code**

``` r

sigma_pars <- c("sigma_layer[1]", "sigma_layer[2]")
sigma_arr <- as.array(fit_sim$raw_fit, pars = sigma_pars)
sigma_chain_meds <- rbindlist(lapply(sigma_pars, function(p) {
  data.table(
    parameter = p,
    Chain = factor(seq_len(dim(sigma_arr)[2])),
    med_val = apply(sigma_arr[, , p, drop = FALSE], 2, median)
  )
}))

sigma_ref <- data.frame(
  parameter = sigma_pars,
  true_val = c(latent_params_sim$sigma_cnty, latent_params_sim$sigma_sch),
  label = sprintf(
    "True~sigma == %.2f",
    c(latent_params_sim$sigma_cnty, latent_params_sim$sigma_sch)
  )
)

bayesplot::mcmc_trace(
  sigma_arr,
  facet_args = list(labeller = ggplot2::as_labeller(c(
    "sigma_layer[1]" = "sigma[County]",
    "sigma_layer[2]" = "sigma[School]"
  ), default = ggplot2::label_parsed))
) +
  geom_hline(
    data = sigma_chain_meds,
    aes(yintercept = med_val, color = Chain),
    linetype = "solid", linewidth = 0.5, alpha = 0.8
  ) +
  geom_hline(
    data = sigma_ref,
    aes(yintercept = true_val),
    color = "firebrick", linetype = "dashed", linewidth = 0.8
  ) +
  geom_label(
    data = sigma_ref,
    aes(x = 100, y = true_val, label = label),
    parse = TRUE, color = "firebrick", fill = ggplot2::alpha("white", 0.75),
    linewidth = NA, vjust = -0.3, hjust = 0, size = 3.2
  ) +
  coord_cartesian(ylim = c(0, 2.5)) +
  theme(legend.position = "none")
```

![](examining_fits_files/figure-html/trace-plot-sigmas-1.png)

------------------------------------------------------------------------

#### County Location Offsets ($`\delta_{\text{county}}`$)

Location offsets represent deviation in vaccination propensity from the
parent region. The following trace plots show offset trajectories for
Scruggs, Simone, and Watson counties.

Indicator arrows show:

- **Red arrows (left edge, iteration 0)**: Expected error direction
  based on observational sampling noise from finite sample draws
  ($`\Delta_{\delta} = -\overline{\Delta\text{logit}}_{\text{cov}}`$).
- **Blue arrows (right edge, iteration 500)**: Realized difference
  between the posterior median estimate and the true latent offset.

**Show plot code**

``` r

loc_info <- canonicalize_locations(locations_sim)
off_layer_arr <- as.array(fit_sim$raw_fit, pars = "off_layer")

county_names <- names(latent_params_sim$off_cnty)
non_root_locs <- loc_info$loc_id[-1]
county_pars <- paste0("off_layer[", match(county_names, non_root_locs), "]")
county_arr <- off_layer_arr[, , county_pars, drop = FALSE]
county_chain_meds <- rbindlist(lapply(county_pars, function(p) {
  data.table(
    parameter = p,
    Chain = factor(seq_len(dim(county_arr)[2])),
    med_val = apply(county_arr[, , p, drop = FALSE], 2, median)
  )
}))

obs <- copy(observations_sim)
phi_st <- latent_params_sim$phi_state
cov <- latent_params_sim$uptake
off_cnty <- latent_params_sim$off_cnty
censor_red <- latent_params_sim$censor_reduction

obs_cnty <- obs[loc_id %in% county_names]
obs_cnty[, mu := {
  c_off <- unname(off_cnty[loc_id])
  phi_c <- plogis(qlogis(phi_st[cohort]) + c_off)
  (1 - phi_c) * cov[11, 2] * censor_red
}, by = loc_id]
obs_cnty[, p_adj := (positive + 0.5) / (sample_n + 1.0)]
obs_cnty[, delta_cov := qlogis(p_adj) - qlogis(mu)]

cnty_errs <- obs_cnty[, .(expected_delta_err = -mean(delta_cov)), by = .(loc_id)]
county_post_meds <- apply(county_arr, 3, median)

county_ref <- data.frame(
  parameter = county_pars,
  loc_id = county_names,
  true_val = unname(latent_params_sim$off_cnty[county_names]),
  post_med = unname(county_post_meds[county_pars]),
  label = sprintf(
    "True~delta == %.2f",
    latent_params_sim$off_cnty[county_names]
  )
)
county_ref <- merge(county_ref, cnty_errs, by = "loc_id")

county_ref$red_arrow_x <- 0
county_ref$red_arrow_yend <- county_ref$true_val + county_ref$expected_delta_err
county_ref$blue_arrow_x <- 500
county_ref$blue_arrow_yend <- county_ref$post_med

bayesplot::mcmc_trace(
  county_arr,
  facet_args = list(
    scales = "fixed",
    labeller = ggplot2::as_labeller(setNames(county_names, county_pars))
  )
) +
  geom_hline(
    data = county_chain_meds,
    aes(yintercept = med_val, color = Chain),
    linetype = "solid", linewidth = 0.5, alpha = 0.8
  ) +
  geom_hline(
    data = county_ref,
    aes(yintercept = true_val),
    color = "firebrick", linetype = "dashed", linewidth = 0.8
  ) +
  geom_segment(
    data = county_ref,
    aes(x = red_arrow_x, xend = red_arrow_x, y = true_val, yend = red_arrow_yend),
    arrow = arrow(length = unit(0.04, "inches"), type = "closed"),
    color = "firebrick", linewidth = 0.8
  ) +
  geom_segment(
    data = county_ref,
    aes(x = blue_arrow_x, xend = blue_arrow_x, y = true_val, yend = blue_arrow_yend),
    arrow = arrow(length = unit(0.04, "inches"), type = "closed"),
    color = "royalblue", linewidth = 0.8
  ) +
  geom_label(
    data = county_ref,
    aes(x = 50, y = true_val, label = label),
    parse = TRUE, color = "firebrick", fill = ggplot2::alpha("white", 0.75),
    linewidth = NA, vjust = -0.3, hjust = 0, size = 3.2
  ) +
  theme(legend.position = "none")
```

![](examining_fits_files/figure-html/trace-plot-county-offsets-1.png)

------------------------------------------------------------------------

#### Force of Vaccination Rate ($`\lambda`$)

The unconstrained rate parameters $`\lambda_{\text{raw}}`$ govern the
speed at which eligible individuals receive vaccine doses once reaching
age eligibility. The following trace plots show posterior draws on the
exponentiated rate scale:

**Show plot code**

``` r

lambda_pars <- c("lambda_raw[1]", "lambda_raw[2]")
lambda_arr <- as.array(fit_sim$raw_fit, pars = lambda_pars)
lambda_chain_meds <- rbindlist(lapply(lambda_pars, function(p) {
  data.table(
    parameter = p,
    Chain = factor(seq_len(dim(lambda_arr)[2])),
    med_val = apply(lambda_arr[, , p, drop = FALSE], 2, median)
  )
}))

lambda_ref <- data.frame(
  parameter = lambda_pars,
  true_val = log(latent_params_sim$lambda),
  label = sprintf("True~lambda == %.1f", latent_params_sim$lambda)
)

bayesplot::mcmc_trace(
  lambda_arr,
  facet_args = list(labeller = ggplot2::as_labeller(c(
    "lambda_raw[1]" = "lambda[1]~(Dose~1)",
    "lambda_raw[2]" = "lambda[2]~(Dose~2)"
  ), default = ggplot2::label_parsed))
) +
  geom_hline(
    data = lambda_chain_meds,
    aes(yintercept = med_val, color = Chain),
    linetype = "solid", linewidth = 0.5, alpha = 0.8
  ) +
  geom_hline(
    data = lambda_ref,
    aes(yintercept = true_val),
    color = "firebrick", linetype = "dashed", linewidth = 0.8
  ) +
  geom_label(
    data = lambda_ref,
    aes(x = 100, y = true_val, label = label),
    parse = TRUE, color = "firebrick", fill = ggplot2::alpha("white", 0.75),
    linewidth = NA, vjust = -0.3, hjust = 0, size = 3.2
  ) +
  coord_cartesian(ylim = c(0.5, 1.5)) +
  scale_y_continuous(
    transform = "exp",
    labels = function(x) sprintf("%.2f", exp(x))
  ) +
  labs(y = "Uptake Rate (exponential scale)") +
  theme(legend.position = "none")
```

![](examining_fits_files/figure-html/trace-plot-lambdas-1.png)

------------------------------------------------------------------------

## Summary and Related Vignettes

- For an overview of estimating the underlying process model and
  predicting coverage, see **[Getting Started with
  imuGAP](https://accidda.github.io/imuGAP/articles/imuGAP.md)**.
- To explore input data preparation and surveillance streams, see
  **[Included Example Datasets and Data
  Validation](https://accidda.github.io/imuGAP/articles/example_data.md)**.
- To learn how `imuGAP` estimates parameters across arbitrary spatial
  hierarchy resolutions, see **[Flexible Location Layers in
  imuGAP](https://accidda.github.io/imuGAP/articles/user_specified_layers.md)**.
