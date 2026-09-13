# imuGAP Model Options

Configures model-side options for `imuGAP` estimation.

## Usage

``` r
imugap_options(
  df = 5L,
  dose_schedule = c(1, 4),
  model = c("default"),
  sigma_layer_scale = 2.5
)
```

## Arguments

- df:

  single positive integer; degrees of freedom to use for the cohort
  B-spline basis expansion (default: 5L).

- dose_schedule:

  an ascending integer vector of ages at which each dose `1..n` becomes
  eligible (default: `c(1, 4)` for 2-dose vaccines).

- model:

  character string specifying the model formulation. Defaults to
  `"default"`, with dispatch to optimized single versus multilayer
  versions within `[sampling()]`

- sigma_layer_scale:

  single positive numeric; scale parameter for the Cauchy prior on layer
  standard deviations `sigma_layer` (default: 2.5).

## Value

a list of imuGAP model options

## Examples

``` r
imugap_options()
#> $df
#> [1] 5
#> 
#> $dose_schedule
#> [1] 1 4
#> 
#> $model
#> [1] "default"
#> 
#> $sigma_layer_scale
#> [1] 2.5
#> 
imugap_options(dose_schedule = c(1, 3))
#> $df
#> [1] 5
#> 
#> $dose_schedule
#> [1] 1 3
#> 
#> $model
#> [1] "default"
#> 
#> $sigma_layer_scale
#> [1] 2.5
#> 
imugap_options(sigma_layer_scale = 1.0)
#> $df
#> [1] 5
#> 
#> $dose_schedule
#> [1] 1 4
#> 
#> $model
#> [1] "default"
#> 
#> $sigma_layer_scale
#> [1] 1
#> 
```
