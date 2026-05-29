# Predict coverage probabilities

Uses the output of `[sampling()]` and a target grid to generate
predicted coverage probabilities.

## Usage

``` r
# S3 method for class 'imugap_fit'
predict(object, target)
```

## Arguments

- object:

  an `imugap_fit` object returned by
  [`sampling()`](https://accidda.github.io/imuGAP/reference/sampling.md)

- target:

  a `[data.frame()]` of target populations to predict for

## Value

A `data.table` with columns `sample_id`, `obs_id`, and `p_obs`
containing the predicted coverage probabilities for each posterior draw
and target observation.

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
