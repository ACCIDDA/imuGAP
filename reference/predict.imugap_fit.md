# Predict coverage probabilities

Uses the output of `[sampling()]` and a target grid to generate
predicted coverage probabilities.

## Usage

``` r
# S3 method for class 'imugap_fit'
predict(object, target, posterior_size = NULL, ...)
```

## Arguments

- object:

  an `imugap_fit` object returned by
  [`sampling()`](https://accidda.github.io/imuGAP/reference/sampling.md)

- target:

  a `[data.frame()]` of target populations to predict for

- posterior_size:

  optional single positive integer. When set, predict over only this
  many draws, taken from the end of each chain (the converged tail).
  Must be a multiple of the number of chains; a value that isn't is
  rounded up to the next multiple, with a warning. Must not exceed the
  number of draws in the fit. Defaults to `NULL`, which uses every draw.

- ...:

  additional arguments (currently ignored)

## Value

An object of class `imugap_predict` wrapping the 3D array of predicted
draws and the canonical target dataset.

## Details

The `[predict()]` method takes an `imugap_fit` object (typically the
output of `[sampling()]`) and a target grid (typically output from
`[create_target()]`), and generates predicted coverage probabilities for
each entry in the target.

The `[predict()]` method can be used to generate estimated coverage for
any location, cohort, or age considered within the bounds of the
original sampling fit. Particularly, this includes enclosing locations
without specific observation data, as long as those locations are
*somewhere* in the locations hierarchy.

By default [`predict()`](https://rdrr.io/r/stats/predict.html) uses
every posterior draw in the fit. Supply `posterior_size` to predict over
a sub-sample taken from the end of each chain; this is how the bundled
`predict_sim` fixture is kept small. The returned draws keep the
per-chain structure (iterations x chains x targets). When a sub-sample
is taken [`predict()`](https://rdrr.io/r/stats/predict.html) warns that
it has not checked whether those draws are adequate (chain mixing,
effective sample size).

## Examples

``` r
# \donttest{
# Load example fit object and target population
data("fit_sim", package = "imuGAP")
data("target_sim", package = "imuGAP")

# Generate predictions over 100 posterior draws
preds <- predict(fit_sim, target = target_sim, posterior_size = 100)
#> Warning: predict() is using a sub-sample of 100 posterior draws and does not check whether it is adequate (chain mixing, effective sample size); verify the sufficiency statistics yourself.
# }
```
