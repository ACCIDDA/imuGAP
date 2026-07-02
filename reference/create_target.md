# Construct a target grid for prediction

Builds a target grid, for use with `[predict.imugap_fit()]`, from
vectors of locations, ages, cohorts, and doses. This is pure
construction and does not reference a fitted model, so it can be called
without a fit (e.g. to expand a request into rows before any fit
exists). To validate a target against a specific fit – or to
canonicalize a target you built yourself as a `data.frame` – use
`[canonicalize_target()]`; `[predict.imugap_fit()]` does this for you.

## Usage

``` r
create_target(
  location,
  age,
  cohort,
  dose,
  mode = c("error", "enumerate", "recycle", "snapshot")
)
```

## Arguments

- location:

  a vector of location IDs to target.

- age:

  vector of ages for which to predict coverage, consistent with
  `[canonicalize_populations()]`.

- cohort:

  vector of cohorts for which to predict coverage, consistent with
  `[canonicalize_populations()]`.

- dose:

  vector of doses for which to predict coverage, consistent with
  `[canonicalize_observations()]`.

- mode:

  one of `"error"` (default), `"enumerate"`, `"recycle"`, or
  `"snapshot"`, controlling how the vector inputs combine:

  - `"error"`: all vector inputs must have the same length.

  - `"enumerate"`: all combinations of the inputs.

  - `"recycle"`: recycle the inputs out to the least-common-multiple
    length.

  - `"snapshot"`: `cohort` must be a single reference value (the oldest
    cohort); locations, ages, and doses are enumerated with a cohort for
    each age such that `age + cohort` is constant, using the **maximum**
    value of `age` to set that constant
    (`cohort_i = cohort_ref + max(age) - age_i`), i.e. a snapshot in
    time.

## Value

a `data.table` target grid with columns `obs_c_id`, `loc_id`, `age`,
`cohort`, `dose`, and `weight`.

## See also

`[canonicalize_target()]`, `[predict.imugap_fit()]`

## Examples

``` r
# "error" mode: all vector inputs must have the same length.
create_target(
  location = c("Blue Heron School", "Bluebird Learning Center"),
  age = c(1, 2), cohort = c(2, 3), dose = c(1, 1), mode = "error"
)
#>    obs_c_id                   loc_id   age cohort  dose weight
#>       <int>                   <char> <num>  <num> <num>  <num>
#> 1:        1        Blue Heron School     1      2     1      1
#> 2:        2 Bluebird Learning Center     2      3     1      1

# "enumerate": all combinations of the inputs.
create_target(
  location = c("Blue Heron School", "Bluebird Learning Center"),
  age = c(1, 2), cohort = c(2, 3), dose = c(1), mode = "enumerate"
)
#>    obs_c_id                   loc_id   age cohort  dose weight
#>       <int>                   <char> <num>  <num> <num>  <num>
#> 1:        1        Blue Heron School     1      2     1      1
#> 2:        2 Bluebird Learning Center     1      2     1      1
#> 3:        3        Blue Heron School     2      2     1      1
#> 4:        4 Bluebird Learning Center     2      2     1      1
#> 5:        5        Blue Heron School     1      3     1      1
#> 6:        6 Bluebird Learning Center     1      3     1      1
#> 7:        7        Blue Heron School     2      3     1      1
#> 8:        8 Bluebird Learning Center     2      3     1      1

# "snapshot": cohort is a single reference; cohorts are set so age + cohort is
# constant, using max(age).
create_target(
  location = c("Blue Heron School", "Bluebird Learning Center"),
  age = c(1, 2, 3), cohort = 5, dose = c(1), mode = "snapshot"
)
#>    obs_c_id                   loc_id   age cohort  dose weight
#>       <int>                   <char> <num>  <num> <num>  <num>
#> 1:        1        Blue Heron School     1      7     1      1
#> 2:        2 Bluebird Learning Center     1      7     1      1
#> 3:        3        Blue Heron School     2      6     1      1
#> 4:        4 Bluebird Learning Center     2      6     1      1
#> 5:        5        Blue Heron School     3      5     1      1
#> 6:        6 Bluebird Learning Center     3      5     1      1
```
