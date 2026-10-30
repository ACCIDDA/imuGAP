# imuGAP Model Options

Configures model-side options for `imuGAP` estimation.

## Usage

``` r
imugap_options(
  df = 5L,
  dose_schedule = c(1, 4),
  compute_log_lik = FALSE,
  time = c("bspline"),
  offsets = c("static"),
  link = c("logit", "probit"),
  solver = c("halley2", "direct", "newton2", "builtin", "halley10")
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
  `"bspline"`). Options include `"bspline"`.

- offsets:

  character string specifying the spatial offset model component
  (default: `"static"`). Options include `"static"`.

- link:

  character string specifying the link function formulation (default:
  `"logit"`). Options include `"logit"` and `"probit"`.

- solver:

  character string specifying the rootfinder solver method (default:
  `"halley2"`). Options include `"halley2"`, `"direct"`, `"newton2"`,
  `"builtin"`, and `"halley10"`.

## Value

an object of class `imugap_options`, wrapping a named list of model
options.

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
#> [1] "bspline_static_zero_halley2_logit"
#> 
#> $solver
#> [1] "halley2"
#> 
#> $solver_type
#> [1] 2
#> 
#> attr(,"class")
#> [1] "imugap_options"
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
#> [1] "bspline_static_zero_halley2_logit"
#> 
#> $solver
#> [1] "halley2"
#> 
#> $solver_type
#> [1] 2
#> 
#> attr(,"class")
#> [1] "imugap_options"
imugap_options(link = "probit", solver = "halley2")
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
#> [1] "bspline_static_zero_halley2_probit"
#> 
#> $solver
#> [1] "halley2"
#> 
#> $solver_type
#> [1] 2
#> 
#> attr(,"class")
#> [1] "imugap_options"
```
