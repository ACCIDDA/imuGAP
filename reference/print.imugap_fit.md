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
#> beta_bs[1]         -1.68    0.00 0.03     -1.74     -1.70     -1.67     -1.65
#> beta_bs[2]         -1.88    0.00 0.05     -1.98     -1.92     -1.88     -1.84
#> beta_bs[3]         -2.48    0.00 0.09     -2.64     -2.54     -2.47     -2.41
#> beta_bs[4]         -3.13    0.00 0.09     -3.30     -3.19     -3.13     -3.06
#> beta_bs[5]         -2.59    0.00 0.09     -2.76     -2.65     -2.58     -2.53
#> sigma_layer[1]      0.52    0.01 0.28      0.19      0.31      0.45      0.67
#> sigma_layer[2]      0.73    0.01 0.12      0.53      0.64      0.72      0.80
#> lambda_raw[1]       1.02    0.00 0.03      0.97      1.00      1.02      1.04
#> lambda_raw[2]       1.06    0.00 0.01      1.04      1.05      1.06      1.07
#> lp__           -78865.34    0.29 5.21 -78876.40 -78868.69 -78864.98 -78861.60
#>                    97.5% n_eff Rhat
#> beta_bs[1]         -1.61  1532 1.00
#> beta_bs[2]         -1.78   922 1.01
#> beta_bs[3]         -2.31  1038 1.01
#> beta_bs[4]         -2.95   989 1.00
#> beta_bs[5]         -2.41  1266 1.00
#> sigma_layer[1]      1.25   638 1.01
#> sigma_layer[2]      0.97   224 1.00
#> lambda_raw[1]       1.07  1503 1.00
#> lambda_raw[2]       1.09  1148 1.00
#> lp__           -78856.27   318 1.00
#> 
#> Samples were drawn using NUTS(diag_e) at Tue Sep 29 23:08:48 2026.
#> For each parameter, n_eff is a crude measure of effective sample size,
#> and Rhat is the potential scale reduction factor on split chains (at 
#> convergence, Rhat=1).
```
