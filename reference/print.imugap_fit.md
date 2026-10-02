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
#>                     mean se_mean   sd      2.5%       25%       50%       75%
#> beta_bs[1]         -1.67    0.00 0.03     -1.74     -1.69     -1.67     -1.65
#> beta_bs[2]         -1.88    0.00 0.05     -1.99     -1.92     -1.88     -1.85
#> beta_bs[3]         -2.48    0.00 0.08     -2.63     -2.53     -2.48     -2.42
#> beta_bs[4]         -3.13    0.00 0.09     -3.32     -3.19     -3.12     -3.05
#> beta_bs[5]         -2.59    0.00 0.08     -2.76     -2.64     -2.59     -2.54
#> sigma_layer[1]      0.73    0.14 0.60      0.19      0.34      0.51      0.83
#> sigma_layer[2]      0.74    0.02 0.14      0.55      0.64      0.73      0.83
#> lambda_raw[1]       1.02    0.00 0.03      0.97      1.00      1.01      1.03
#> lambda_raw[2]       1.06    0.00 0.01      1.04      1.05      1.06      1.07
#> lp__           -78865.34    0.45 5.32 -78875.41 -78869.42 -78865.13 -78861.48
#>                    97.5% n_eff Rhat
#> beta_bs[1]         -1.61   158 1.03
#> beta_bs[2]         -1.78   476 1.01
#> beta_bs[3]         -2.31   686 1.00
#> beta_bs[4]         -2.96   399 1.02
#> beta_bs[5]         -2.42   940 1.01
#> sigma_layer[1]      2.41    18 1.28
#> sigma_layer[2]      1.05    84 1.06
#> lambda_raw[1]       1.07   888 1.01
#> lambda_raw[2]       1.09   411 1.01
#> lp__           -78855.54   142 1.04
#> 
#> Samples were drawn using NUTS(diag_e) at Fri Oct  2 14:52:18 2026.
#> For each parameter, n_eff is a crude measure of effective sample size,
#> and Rhat is the potential scale reduction factor on split chains (at 
#> convergence, Rhat=1).
```
