# Create target population for prediction

Creates a target object appropriate for use with
`[predict.imugap_fit()]`, which will be used with a specific
`imugap_fit` object.

## Usage

``` r
create_target(
  fit,
  location,
  age,
  cohort,
  dose,
  mode = c("error", "enumerate", "recycle")
)
```

## Arguments

- fit:

  an `imugap_fit` object returned by `[sampling()]`

- location:

  either a vector of locations or a `data.frame`; if a vector of
  locations, treated as the target locations; if a `data.frame`, then
  validated as the target object.

- age:

  vector of ages for which to predict coverage, consistent with
  `[canonicalize_populations()]`

- cohort:

  vector of cohorts for which to predict coverage, consistent with
  `[canonicalize_populations()]`

- dose:

  vector of doses for which to predict coverage, consistent with
  `[canonicalize_observations()]`

## Value

A `data.table` representing the canonicalized target population.

## Details

When `location` is a `data.frame`, this function validates that object
against the `fit` argument. Non-missing values for the other arguments
are an error for that approach.

Otherwise, `location` must correspond to a vector of location IDs and
`age`, `cohort`, and `dose` must also be supplied. Depending on the
`mode` argument, these arguments may have different lengths. If
`mode = "error"` (default), then all of these arguments must have the
same length. If `mode = "enumerate"`, then the resulting target will be
all combinations of the arguments. If `mode = "recycle"`, then the
resulting target will recycle all the arguments out to the
least-common-multiple length.
