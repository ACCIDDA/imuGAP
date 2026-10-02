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
