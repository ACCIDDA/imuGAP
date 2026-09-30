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
#> beta_bs[1]         -1.68    0.00 0.03     -1.74     -1.70     -1.68     -1.66
#> beta_bs[2]         -1.88    0.00 0.05     -1.98     -1.92     -1.88     -1.85
#> beta_bs[3]         -2.48    0.00 0.09     -2.65     -2.54     -2.47     -2.42
#> beta_bs[4]         -3.13    0.00 0.09     -3.34     -3.19     -3.13     -3.07
#> beta_bs[5]         -2.59    0.00 0.09     -2.76     -2.65     -2.60     -2.54
#> sigma_layer[1]      0.59    0.04 0.36      0.19      0.33      0.48      0.74
#> sigma_layer[2]      0.75    0.01 0.14      0.54      0.65      0.73      0.83
#> lambda_raw[1]       1.02    0.00 0.03      0.97      1.00      1.02      1.03
#> lambda_raw[2]       1.06    0.00 0.01      1.04      1.05      1.06      1.07
#> lp__           -78864.68    0.52 5.59 -78876.24 -78868.39 -78864.50 -78860.68
#>                    97.5% n_eff Rhat
#> beta_bs[1]         -1.61  1181 1.00
#> beta_bs[2]         -1.78   622 1.01
#> beta_bs[3]         -2.31   967 1.00
#> beta_bs[4]         -2.95   724 1.01
#> beta_bs[5]         -2.42  1236 1.00
#> sigma_layer[1]      1.57    68 1.05
#> sigma_layer[2]      1.07   141 1.02
#> lambda_raw[1]       1.07   508 1.01
#> lambda_raw[2]       1.09  1031 1.00
#> lp__           -78854.78   115 1.02
#> 
#> Samples were drawn using NUTS(diag_e) at Wed Sep 30 17:56:21 2026.
#> For each parameter, n_eff is a crude measure of effective sample size,
#> and Rhat is the potential scale reduction factor on split chains (at 
#> convergence, Rhat=1).
```
