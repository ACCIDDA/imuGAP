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
  mode = c("error", "enumerate", "recycle", "snapshot")
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
least-common-multiple length. If `mode = "snapshot"`, then the cohort
argument must be a single reference value (which represents the oldest
cohort). The resulting target will enumerate combinations of locations,
ages, and doses, calculating cohorts for each age such that the sum of
age and cohort is constant (i.e., cohort_i = cohort_ref + max_age -
age_i). This corresponds to a snapshot in time of different birth date
and age combinations.

## Examples

``` r
# Load example fit object
data("fit_sim")

# 1. Default "error" mode: All vector inputs must have the same length.
create_target(
  fit = fit_sim,
  location = c("Blue Heron School", "Bluebird Learning Center"),
  age = c(1, 2),
  cohort = c(2, 3),
  dose = c(1, 1),
  mode = "error"
)
#>    obs_c_id                   loc_id   age cohort  dose weight loc_c_id
#>       <int>                   <char> <num>  <num> <num>  <num>    <int>
#> 1:        1        Blue Heron School     1      2     1      1        5
#> 2:        2 Bluebird Learning Center     2      3     1      1        6

# Providing mismatched length arguments in "error" mode throws an error:
try(create_target(
  fit = fit_sim,
  location = c("Blue Heron School", "Bluebird Learning Center"),
  age = c(1), # length mismatch
  cohort = c(2, 3),
  dose = c(1, 1),
  mode = "error"
))
#> Error : All arguments must have the same length in 'error' mode

# 2. "enumerate" mode: Generates all combinations of the arguments.
create_target(
  fit = fit_sim,
  location = c("Blue Heron School", "Bluebird Learning Center"),
  age = c(1, 2),
  cohort = c(2, 3),
  dose = c(1),
  mode = "enumerate"
)
#>    obs_c_id                   loc_id   age cohort  dose weight loc_c_id
#>       <int>                   <char> <num>  <num> <num>  <num>    <int>
#> 1:        1        Blue Heron School     1      2     1      1        5
#> 2:        2 Bluebird Learning Center     1      2     1      1        6
#> 3:        3        Blue Heron School     2      2     1      1        5
#> 4:        4 Bluebird Learning Center     2      2     1      1        6
#> 5:        5        Blue Heron School     1      3     1      1        5
#> 6:        6 Bluebird Learning Center     1      3     1      1        6
#> 7:        7        Blue Heron School     2      3     1      1        5
#> 8:        8 Bluebird Learning Center     2      3     1      1        6

# 3. "recycle" mode: Recycles arguments to the least-common-multiple length.
create_target(
  fit = fit_sim,
  location = c("Blue Heron School", "Bluebird Learning Center"),
  age = c(1, 2, 3),
  cohort = c(2),
  dose = c(1),
  mode = "recycle"
)
#>    obs_c_id                   loc_id   age cohort  dose weight loc_c_id
#>       <int>                   <char> <num>  <num> <num>  <num>    <int>
#> 1:        1        Blue Heron School     1      2     1      1        5
#> 2:        2 Bluebird Learning Center     2      2     1      1        6
#> 3:        3        Blue Heron School     3      2     1      1        5
#> 4:        4 Bluebird Learning Center     1      2     1      1        6
#> 5:        5        Blue Heron School     2      2     1      1        5
#> 6:        6 Bluebird Learning Center     3      2     1      1        6

# 4. "snapshot" mode: Cohort is a single reference value. Cohorts for each
# age are calculated so that cohort + age is constant (representing a snapshot in time).
create_target(
  fit = fit_sim,
  location = c("Blue Heron School", "Bluebird Learning Center"),
  age = c(1, 2, 3),
  cohort = 5,
  dose = c(1),
  mode = "snapshot"
)
#>    obs_c_id                   loc_id   age cohort  dose weight loc_c_id
#>       <int>                   <char> <num>  <num> <num>  <num>    <int>
#> 1:        1        Blue Heron School     1      7     1      1        5
#> 2:        2 Bluebird Learning Center     1      7     1      1        6
#> 3:        3        Blue Heron School     2      6     1      1        5
#> 4:        4 Bluebird Learning Center     2      6     1      1        6
#> 5:        5        Blue Heron School     3      5     1      1        5
#> 6:        6 Bluebird Learning Center     3      5     1      1        6

# 5. Providing a data.frame for validation.
df_target <- data.frame(
  loc_id = c("Blue Heron School", "Bluebird Learning Center"),
  age = c(1, 2),
  cohort = c(2, 3),
  dose = c(1, 1)
)
create_target(fit = fit_sim, location = df_target)
#>    obs_c_id                   loc_id   age cohort  dose weight loc_c_id
#>       <int>                   <char> <num>  <num> <num>  <num>    <int>
#> 1:        1        Blue Heron School     1      2     1      1        5
#> 2:        2 Bluebird Learning Center     2      3     1      1        6
```
