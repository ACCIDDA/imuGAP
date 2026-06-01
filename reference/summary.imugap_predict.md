# Summarize coverage predictions

Summarizes predicted coverage probabilities from an `imugap_predict`
object by location, cohort, age, and dose for the requested quantiles.

## Usage

``` r
# S3 method for class 'imugap_predict'
summary(object, probs = c(0.025, 0.5, 0.975), ...)
```

## Arguments

- object:

  an `imugap_predict` object returned by `[predict()]`

- probs:

  numeric vector of probabilities/quantiles to compute. Defaults to
  `c(0.025, 0.5, 0.975)`.

- ...:

  additional arguments (currently ignored)

## Value

A `data.table` containing target population parameters, posterior mean
coverage (`mean`), and the requested quantiles (e.g. `q2.5`, `q50`,
`q97.5`).
