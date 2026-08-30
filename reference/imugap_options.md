# imuGAP Model Options

This function encapsulates option passing for imuGAP settings.

## Usage

``` r
imugap_options(df = 5L, dose_schedule = c(1, 4), model = c("default"))
```

## Arguments

- df:

  degrees of freedom to use in bspline

- dose_schedule:

  an integer vector, the ages at which dose(s) `n` are scheduled, with
  vector indices and doses matching

- model:

  which model formulation to use; currently "default" is supported

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
```
