# imuGAP Model Options

Configures model-side options for `imuGAP` estimation.

## Usage

``` r
imugap_options(
  df = 5L,
  dose_schedule = c(1, 4),
  compute_log_lik = FALSE,
  time = c("default", "bspline"),
  offsets = c("default", "static"),
  link = c("default", "logit", "probit"),
  model = NULL
)
```

## Arguments

- df:

  single positive integer; degrees of freedom to use for the cohort
  B-spline basis expansion (default: 5L).

- dose_schedule:

  ascending integer vector of ages at which each dose `1..n` becomes
  eligible (default: `c(1, 4)` for 2-dose vaccines).

- compute_log_lik:

  logical scalar; compute pointwise log-likelihood during sampling?
  (default: `FALSE`).

- time:

  character string specifying the temporal model component (default:
  `"default"`, resolving to `"bspline"`).

- offsets:

  character string specifying the spatial offset model component
  (default: `"default"`, resolving to `"static"`).

- link:

  character string specifying the link function formulation (default:
  `"default"`, resolving to `"logit"`). Options include `"default"` (or
  `"logit"`) and `"probit"`.

- model:

  optional character string; alias for `link` or full model template
  name (default: `NULL`).

## Value

a named list, of `imuGAP` model options.

## Examples

``` r
imugap_options()
#> $df
#> [1] 5
#> 
#> $dose_schedule
#> [1] 1 4
#> 
#> $compute_log_lik
#> [1] FALSE
#> 
#> $time
#> [1] "bspline"
#> 
#> $offsets
#> [1] "static"
#> 
#> $link
#> [1] "logit"
#> 
#> $model
#> [1] "bspline_static_logit"
#> 
imugap_options(dose_schedule = c(1, 3))
#> $df
#> [1] 5
#> 
#> $dose_schedule
#> [1] 1 3
#> 
#> $compute_log_lik
#> [1] FALSE
#> 
#> $time
#> [1] "bspline"
#> 
#> $offsets
#> [1] "static"
#> 
#> $link
#> [1] "logit"
#> 
#> $model
#> [1] "bspline_static_logit"
#> 
imugap_options(link = "probit")
#> $df
#> [1] 5
#> 
#> $dose_schedule
#> [1] 1 4
#> 
#> $compute_log_lik
#> [1] FALSE
#> 
#> $time
#> [1] "bspline"
#> 
#> $offsets
#> [1] "static"
#> 
#> $link
#> [1] "probit"
#> 
#> $model
#> [1] "bspline_static_probit"
#> 
```
