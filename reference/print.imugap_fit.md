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
#> Inference for Stan model: bspline_static_logit.
#> 4 chains, each with iter=1000; warmup=500; thin=1; 
#> post-warmup draws per chain=500, total post-warmup draws=2000.
#> 
#>                       mean se_mean   sd      2.5%       25%       50%       75%
#> beta_bs[1]           -1.67    0.01 0.03     -1.74     -1.69     -1.66     -1.65
#> beta_bs[2]           -1.89    0.01 0.05     -1.99     -1.92     -1.89     -1.85
#> beta_bs[3]           -2.47    0.01 0.08     -2.64     -2.53     -2.48     -2.41
#> beta_bs[4]           -3.14    0.02 0.09     -3.31     -3.20     -3.15     -3.07
#> beta_bs[5]           -2.58    0.01 0.08     -2.76     -2.63     -2.58     -2.53
#> sigma_layer[1]        0.77    0.21 0.49      0.21      0.37      0.60      1.20
#> sigma_layer[2]        0.77    0.05 0.12      0.55      0.68      0.77      0.89
#> lambda_raw[1]         1.02    0.00 0.02      0.97      1.01      1.02      1.03
#> lambda_raw[2]         1.06    0.00 0.01      1.04      1.05      1.06      1.07
#> raw_phi_root[1]      -1.67    0.01 0.03     -1.74     -1.69     -1.66     -1.65
#> raw_phi_root[2]      -1.72    0.00 0.02     -1.77     -1.73     -1.71     -1.70
#> raw_phi_root[3]      -1.76    0.00 0.02     -1.81     -1.78     -1.76     -1.75
#> raw_phi_root[4]      -1.81    0.00 0.02     -1.86     -1.83     -1.82     -1.80
#> raw_phi_root[5]      -1.87    0.00 0.02     -1.91     -1.88     -1.87     -1.85
#> raw_phi_root[6]      -1.92    0.00 0.02     -1.96     -1.94     -1.92     -1.90
#> raw_phi_root[7]      -1.98    0.00 0.02     -2.02     -1.99     -1.98     -1.96
#> raw_phi_root[8]      -2.03    0.00 0.02     -2.07     -2.05     -2.04     -2.02
#> raw_phi_root[9]      -2.09    0.00 0.03     -2.13     -2.11     -2.09     -2.07
#> raw_phi_root[10]     -2.15    0.00 0.03     -2.19     -2.17     -2.15     -2.13
#> raw_phi_root[11]     -2.21    0.00 0.03     -2.26     -2.23     -2.21     -2.19
#> raw_phi_root[12]     -2.27    0.00 0.03     -2.32     -2.29     -2.27     -2.25
#> raw_phi_root[13]     -2.33    0.00 0.03     -2.39     -2.36     -2.33     -2.31
#> raw_phi_root[14]     -2.40    0.00 0.03     -2.45     -2.42     -2.40     -2.38
#> raw_phi_root[15]     -2.46    0.00 0.03     -2.52     -2.49     -2.46     -2.44
#> raw_phi_root[16]     -2.52    0.00 0.04     -2.59     -2.55     -2.53     -2.50
#> raw_phi_root[17]     -2.59    0.00 0.04     -2.65     -2.62     -2.59     -2.56
#> raw_phi_root[18]     -2.65    0.01 0.04     -2.72     -2.68     -2.65     -2.62
#> raw_phi_root[19]     -2.71    0.01 0.04     -2.78     -2.74     -2.71     -2.68
#> raw_phi_root[20]     -2.76    0.01 0.04     -2.84     -2.79     -2.76     -2.73
#> raw_phi_root[21]     -2.81    0.01 0.05     -2.89     -2.84     -2.81     -2.78
#> raw_phi_root[22]     -2.84    0.01 0.05     -2.92     -2.88     -2.84     -2.81
#> raw_phi_root[23]     -2.87    0.01 0.05     -2.95     -2.90     -2.87     -2.83
#> raw_phi_root[24]     -2.88    0.01 0.05     -2.96     -2.91     -2.88     -2.85
#> raw_phi_root[25]     -2.88    0.01 0.05     -2.96     -2.91     -2.88     -2.84
#> raw_phi_root[26]     -2.86    0.00 0.04     -2.94     -2.89     -2.86     -2.83
#> raw_phi_root[27]     -2.82    0.00 0.04     -2.91     -2.85     -2.82     -2.79
#> raw_phi_root[28]     -2.76    0.00 0.05     -2.86     -2.79     -2.76     -2.74
#> raw_phi_root[29]     -2.68    0.00 0.06     -2.82     -2.72     -2.68     -2.65
#> raw_phi_root[30]     -2.58    0.01 0.08     -2.76     -2.63     -2.58     -2.53
#> mu_offset[1,1]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[1,2]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[1,3]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[1,4]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[2,1]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[2,2]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[2,3]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[2,4]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[3,1]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[3,2]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[3,3]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[3,4]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[4,1]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[4,2]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[4,3]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[4,4]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[5,1]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[5,2]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[5,3]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[5,4]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[6,1]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[6,2]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[6,3]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[6,4]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[7,1]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[7,2]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[7,3]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[7,4]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[8,1]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[8,2]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[8,3]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[8,4]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[9,1]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[9,2]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[9,3]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[9,4]        0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[10,1]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[10,2]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[10,3]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[10,4]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[11,1]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[11,2]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[11,3]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[11,4]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[12,1]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[12,2]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[12,3]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[12,4]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[13,1]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[13,2]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[13,3]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[13,4]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[14,1]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[14,2]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[14,3]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[14,4]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[15,1]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[15,2]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[15,3]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[15,4]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[16,1]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[16,2]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[16,3]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[16,4]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[17,1]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[17,2]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[17,3]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[17,4]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[18,1]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[18,2]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[18,3]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[18,4]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[19,1]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[19,2]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[19,3]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[19,4]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[20,1]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[20,2]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[20,3]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[20,4]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[21,1]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[21,2]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[21,3]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[21,4]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[22,1]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[22,2]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[22,3]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[22,4]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[23,1]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[23,2]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[23,3]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[23,4]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[24,1]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[24,2]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[24,3]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[24,4]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[25,1]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[25,2]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[25,3]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[25,4]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[26,1]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[26,2]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[26,3]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[26,4]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[27,1]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[27,2]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[27,3]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[27,4]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[28,1]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[28,2]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[28,3]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[28,4]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[29,1]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[29,2]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[29,3]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[29,4]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[30,1]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[30,2]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[30,3]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> mu_offset[30,4]       0.00     NaN 0.00      0.00      0.00      0.00      0.00
#> lp__             -78863.18    2.42 5.68 -78874.52 -78867.13 -78863.18 -78858.38
#>                      97.5% n_eff Rhat
#> beta_bs[1]           -1.61    39 1.05
#> beta_bs[2]           -1.78    24 1.09
#> beta_bs[3]           -2.31   126 1.05
#> beta_bs[4]           -2.94    24 1.09
#> beta_bs[5]           -2.41   194 1.04
#> sigma_layer[1]        1.78     5 1.45
#> sigma_layer[2]        0.97     7 1.25
#> lambda_raw[1]         1.07   799 1.01
#> lambda_raw[2]         1.09   146 1.03
#> raw_phi_root[1]      -1.61    39 1.05
#> raw_phi_root[2]      -1.67   623 1.02
#> raw_phi_root[3]      -1.72   803 1.01
#> raw_phi_root[4]      -1.77   381 1.02
#> raw_phi_root[5]      -1.82   141 1.04
#> raw_phi_root[6]      -1.87    86 1.05
#> raw_phi_root[7]      -1.92    59 1.05
#> raw_phi_root[8]      -1.98    52 1.05
#> raw_phi_root[9]      -2.04    52 1.05
#> raw_phi_root[10]     -2.09    59 1.05
#> raw_phi_root[11]     -2.15    96 1.04
#> raw_phi_root[12]     -2.21   112 1.05
#> raw_phi_root[13]     -2.27   121 1.05
#> raw_phi_root[14]     -2.33   113 1.05
#> raw_phi_root[15]     -2.39    98 1.06
#> raw_phi_root[16]     -2.45    82 1.07
#> raw_phi_root[17]     -2.52    67 1.08
#> raw_phi_root[18]     -2.57    56 1.09
#> raw_phi_root[19]     -2.63    49 1.09
#> raw_phi_root[20]     -2.68    43 1.10
#> raw_phi_root[21]     -2.72    39 1.10
#> raw_phi_root[22]     -2.75    36 1.09
#> raw_phi_root[23]     -2.77    35 1.08
#> raw_phi_root[24]     -2.78    38 1.07
#> raw_phi_root[25]     -2.78    47 1.06
#> raw_phi_root[26]     -2.76    92 1.04
#> raw_phi_root[27]     -2.72   643 1.01
#> raw_phi_root[28]     -2.66   740 1.01
#> raw_phi_root[29]     -2.56   595 1.02
#> raw_phi_root[30]     -2.41   194 1.04
#> mu_offset[1,1]        0.00   NaN  NaN
#> mu_offset[1,2]        0.00   NaN  NaN
#> mu_offset[1,3]        0.00   NaN  NaN
#> mu_offset[1,4]        0.00   NaN  NaN
#> mu_offset[2,1]        0.00   NaN  NaN
#> mu_offset[2,2]        0.00   NaN  NaN
#> mu_offset[2,3]        0.00   NaN  NaN
#> mu_offset[2,4]        0.00   NaN  NaN
#> mu_offset[3,1]        0.00   NaN  NaN
#> mu_offset[3,2]        0.00   NaN  NaN
#> mu_offset[3,3]        0.00   NaN  NaN
#> mu_offset[3,4]        0.00   NaN  NaN
#> mu_offset[4,1]        0.00   NaN  NaN
#> mu_offset[4,2]        0.00   NaN  NaN
#> mu_offset[4,3]        0.00   NaN  NaN
#> mu_offset[4,4]        0.00   NaN  NaN
#> mu_offset[5,1]        0.00   NaN  NaN
#> mu_offset[5,2]        0.00   NaN  NaN
#> mu_offset[5,3]        0.00   NaN  NaN
#> mu_offset[5,4]        0.00   NaN  NaN
#> mu_offset[6,1]        0.00   NaN  NaN
#> mu_offset[6,2]        0.00   NaN  NaN
#> mu_offset[6,3]        0.00   NaN  NaN
#> mu_offset[6,4]        0.00   NaN  NaN
#> mu_offset[7,1]        0.00   NaN  NaN
#> mu_offset[7,2]        0.00   NaN  NaN
#> mu_offset[7,3]        0.00   NaN  NaN
#> mu_offset[7,4]        0.00   NaN  NaN
#> mu_offset[8,1]        0.00   NaN  NaN
#> mu_offset[8,2]        0.00   NaN  NaN
#> mu_offset[8,3]        0.00   NaN  NaN
#> mu_offset[8,4]        0.00   NaN  NaN
#> mu_offset[9,1]        0.00   NaN  NaN
#> mu_offset[9,2]        0.00   NaN  NaN
#> mu_offset[9,3]        0.00   NaN  NaN
#> mu_offset[9,4]        0.00   NaN  NaN
#> mu_offset[10,1]       0.00   NaN  NaN
#> mu_offset[10,2]       0.00   NaN  NaN
#> mu_offset[10,3]       0.00   NaN  NaN
#> mu_offset[10,4]       0.00   NaN  NaN
#> mu_offset[11,1]       0.00   NaN  NaN
#> mu_offset[11,2]       0.00   NaN  NaN
#> mu_offset[11,3]       0.00   NaN  NaN
#> mu_offset[11,4]       0.00   NaN  NaN
#> mu_offset[12,1]       0.00   NaN  NaN
#> mu_offset[12,2]       0.00   NaN  NaN
#> mu_offset[12,3]       0.00   NaN  NaN
#> mu_offset[12,4]       0.00   NaN  NaN
#> mu_offset[13,1]       0.00   NaN  NaN
#> mu_offset[13,2]       0.00   NaN  NaN
#> mu_offset[13,3]       0.00   NaN  NaN
#> mu_offset[13,4]       0.00   NaN  NaN
#> mu_offset[14,1]       0.00   NaN  NaN
#> mu_offset[14,2]       0.00   NaN  NaN
#> mu_offset[14,3]       0.00   NaN  NaN
#> mu_offset[14,4]       0.00   NaN  NaN
#> mu_offset[15,1]       0.00   NaN  NaN
#> mu_offset[15,2]       0.00   NaN  NaN
#> mu_offset[15,3]       0.00   NaN  NaN
#> mu_offset[15,4]       0.00   NaN  NaN
#> mu_offset[16,1]       0.00   NaN  NaN
#> mu_offset[16,2]       0.00   NaN  NaN
#> mu_offset[16,3]       0.00   NaN  NaN
#> mu_offset[16,4]       0.00   NaN  NaN
#> mu_offset[17,1]       0.00   NaN  NaN
#> mu_offset[17,2]       0.00   NaN  NaN
#> mu_offset[17,3]       0.00   NaN  NaN
#> mu_offset[17,4]       0.00   NaN  NaN
#> mu_offset[18,1]       0.00   NaN  NaN
#> mu_offset[18,2]       0.00   NaN  NaN
#> mu_offset[18,3]       0.00   NaN  NaN
#> mu_offset[18,4]       0.00   NaN  NaN
#> mu_offset[19,1]       0.00   NaN  NaN
#> mu_offset[19,2]       0.00   NaN  NaN
#> mu_offset[19,3]       0.00   NaN  NaN
#> mu_offset[19,4]       0.00   NaN  NaN
#> mu_offset[20,1]       0.00   NaN  NaN
#> mu_offset[20,2]       0.00   NaN  NaN
#> mu_offset[20,3]       0.00   NaN  NaN
#> mu_offset[20,4]       0.00   NaN  NaN
#> mu_offset[21,1]       0.00   NaN  NaN
#> mu_offset[21,2]       0.00   NaN  NaN
#> mu_offset[21,3]       0.00   NaN  NaN
#> mu_offset[21,4]       0.00   NaN  NaN
#> mu_offset[22,1]       0.00   NaN  NaN
#> mu_offset[22,2]       0.00   NaN  NaN
#> mu_offset[22,3]       0.00   NaN  NaN
#> mu_offset[22,4]       0.00   NaN  NaN
#> mu_offset[23,1]       0.00   NaN  NaN
#> mu_offset[23,2]       0.00   NaN  NaN
#> mu_offset[23,3]       0.00   NaN  NaN
#> mu_offset[23,4]       0.00   NaN  NaN
#> mu_offset[24,1]       0.00   NaN  NaN
#> mu_offset[24,2]       0.00   NaN  NaN
#> mu_offset[24,3]       0.00   NaN  NaN
#> mu_offset[24,4]       0.00   NaN  NaN
#> mu_offset[25,1]       0.00   NaN  NaN
#> mu_offset[25,2]       0.00   NaN  NaN
#> mu_offset[25,3]       0.00   NaN  NaN
#> mu_offset[25,4]       0.00   NaN  NaN
#> mu_offset[26,1]       0.00   NaN  NaN
#> mu_offset[26,2]       0.00   NaN  NaN
#> mu_offset[26,3]       0.00   NaN  NaN
#> mu_offset[26,4]       0.00   NaN  NaN
#> mu_offset[27,1]       0.00   NaN  NaN
#> mu_offset[27,2]       0.00   NaN  NaN
#> mu_offset[27,3]       0.00   NaN  NaN
#> mu_offset[27,4]       0.00   NaN  NaN
#> mu_offset[28,1]       0.00   NaN  NaN
#> mu_offset[28,2]       0.00   NaN  NaN
#> mu_offset[28,3]       0.00   NaN  NaN
#> mu_offset[28,4]       0.00   NaN  NaN
#> mu_offset[29,1]       0.00   NaN  NaN
#> mu_offset[29,2]       0.00   NaN  NaN
#> mu_offset[29,3]       0.00   NaN  NaN
#> mu_offset[29,4]       0.00   NaN  NaN
#> mu_offset[30,1]       0.00   NaN  NaN
#> mu_offset[30,2]       0.00   NaN  NaN
#> mu_offset[30,3]       0.00   NaN  NaN
#> mu_offset[30,4]       0.00   NaN  NaN
#> lp__             -78853.96     5 1.29
#> 
#> Samples were drawn using NUTS(diag_e) at Sat Oct 17 13:54:03 2026.
#> For each parameter, n_eff is a crude measure of effective sample size,
#> and Rhat is the potential scale reduction factor on split chains (at 
#> convergence, Rhat=1).
```
