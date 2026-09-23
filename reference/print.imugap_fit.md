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
#> beta_bs[4]         -3.13    0.00 0.09     -3.30     -3.19     -3.13     -3.07
#> beta_bs[5]         -2.59    0.00 0.08     -2.75     -2.64     -2.59     -2.53
#> sigma_layer[1]      0.56    0.01 0.30      0.19      0.33      0.48      0.72
#> sigma_layer[2]      0.74    0.01 0.12      0.55      0.66      0.73      0.81
#> lambda_raw[1]       1.02    0.00 0.03      0.97      1.00      1.02      1.04
#> lambda_raw[2]       1.06    0.00 0.01      1.03      1.05      1.06      1.07
#> lp__           -78864.85    0.28 5.07 -78875.23 -78868.18 -78864.55 -78861.15
#>                    97.5% n_eff Rhat
#> beta_bs[1]         -1.61  1194 1.00
#> beta_bs[2]         -1.79  1095 1.00
#> beta_bs[3]         -2.31   994 1.00
#> beta_bs[4]         -2.96  1010 1.00
#> beta_bs[5]         -2.43  1403 1.00
#> sigma_layer[1]      1.34   656 1.00
#> sigma_layer[2]      1.03   286 1.01
#> lambda_raw[1]       1.07  1198 1.00
#> lambda_raw[2]       1.09   936 1.00
#> lp__           -78856.26   330 1.00
#> 
#> Samples were drawn using NUTS(diag_e) at Wed Sep 23 01:08:11 2026.
#> For each parameter, n_eff is a crude measure of effective sample size,
#> and Rhat is the potential scale reduction factor on split chains (at 
#> convergence, Rhat=1).
```
