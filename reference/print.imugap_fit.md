# Print an imuGAP model fit

Prints a concise summary of an `imugap_fit` object, including the
location hierarchy dimensions, observation counts, and MCMC parameter
summaries for all non-offset parameters.

## Usage

``` r
# S3 method for class 'imugap_fit'
print(x, pars = NULL, ...)
```

## Arguments

- x:

  an object of class `imugap_fit` returned by `[sampling()]`.

- pars:

  character vector; parameter names to display (default: all non-offset
  parameters, excluding `'z_layer'`).

- ...:

  additional arguments passed to the underlying backend print method.

## Value

invisibly returns `x`.

## Examples

``` r
data("fit_sim", package = "imuGAP")
print(fit_sim)
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
