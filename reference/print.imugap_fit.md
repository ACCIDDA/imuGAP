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
#> beta_bs[1]         -1.68    0.00 0.03     -1.75     -1.70     -1.67     -1.65
#> beta_bs[2]         -1.88    0.00 0.05     -1.98     -1.91     -1.88     -1.84
#> beta_bs[3]         -2.48    0.00 0.09     -2.64     -2.54     -2.48     -2.42
#> beta_bs[4]         -3.12    0.00 0.10     -3.32     -3.19     -3.12     -3.06
#> beta_bs[5]         -2.59    0.00 0.09     -2.76     -2.64     -2.59     -2.53
#> sigma_layer[1]      0.54    0.02 0.29      0.18      0.32      0.47      0.69
#> sigma_layer[2]      0.74    0.01 0.12      0.54      0.65      0.72      0.81
#> lambda_raw[1]       1.02    0.00 0.03      0.97      1.00      1.02      1.04
#> lambda_raw[2]       1.06    0.00 0.02      1.03      1.05      1.06      1.07
#> lp__           -78865.21    0.27 5.22 -78876.68 -78868.27 -78864.85 -78861.52
#>                    97.5% n_eff Rhat
#> beta_bs[1]         -1.61  1374 1.00
#> beta_bs[2]         -1.78   911 1.00
#> beta_bs[3]         -2.31   926 1.00
#> beta_bs[4]         -2.93   761 1.01
#> beta_bs[5]         -2.42  1386 1.00
#> sigma_layer[1]      1.26   380 1.02
#> sigma_layer[2]      1.00   253 1.00
#> lambda_raw[1]       1.07  1106 1.01
#> lambda_raw[2]       1.09   933 1.00
#> lp__           -78856.17   362 1.00
#> 
#> Samples were drawn using NUTS(diag_e) at Thu Oct  1 20:28:29 2026.
#> For each parameter, n_eff is a crude measure of effective sample size,
#> and Rhat is the potential scale reduction factor on split chains (at 
#> convergence, Rhat=1).
```
