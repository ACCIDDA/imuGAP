# imuGAP Model Options

This function encapsulates option passing for imuGAP settings.

## Usage

``` r
imugap_options(df = 5L, dose_schedule = c(1, 4), object = c("default"))
```

## Arguments

- df:

  degrees of freedom to use in bspline

- dose_schedule:

  an integer vector, the ages at which dose(s) `n` are scheduled, with
  vector indices and doses matching

- object:

  which stan model object to use; currently only "default" is supported

## Value

a list of imuGAP model options
