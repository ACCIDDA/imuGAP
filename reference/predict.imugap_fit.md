# Predict coverage probabilities

Uses the output of `[sampling()]` and a target `populations` grid to
generate predicted coverage probabilities.

## Usage

``` r
# S3 method for class 'imugap_fit'
predict(object, target)
```

## Arguments

- object:

  an `imugap_fit` object returned by
  [`sampling()`](https://accidda.github.io/imuGAP/reference/sampling.md)

- populations:

  a `[data.frame()]` of target populations to predict for

- ...:

  additional arguments passed to other methods.

## Value

A `data.table` with columns `sample_id`, `obs_id`, and `p_obs`
containing the predicted coverage probabilities for each posterior draw
and target observation.
