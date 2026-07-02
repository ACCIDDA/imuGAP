# Canonicalize a target grid against a fit

Normalizes a target grid and validates it against a specific
`imugap_fit`.

## Usage

``` r
canonicalize_target(target, fit)
```

## Arguments

- target:

  a target grid: the output of `[create_target()]`, or a `data.frame` /
  `data.table` with `loc_id`, `age`, `cohort`, and `dose` columns.

- fit:

  an `imugap_fit` object returned by `[sampling()]`.

## Value

the validated `target` (a `data.table`) with `loc_c_id` added.

## Details

Accepts either the output of `[create_target()]` or a plain `data.frame`
/ `data.table` with `loc_id`, `age`, `cohort`, and `dose` columns
(optionally `weight` / `obs_id`). Fills `obs_c_id` and `weight` when
absent, checks that every `loc_id` exists in the fit and that `dose`,
`age`, and `cohort` are within the fit's ranges, and adds the canonical
`loc_c_id`. Errors on any out-of-range value. `[predict.imugap_fit()]`
calls this internally, so most users do not call it directly.

## See also

`[create_target()]`, `[predict.imugap_fit()]`

## Examples

``` r
data("fit_sim")
target <- create_target(
  location = c("Blue Heron School", "Bluebird Learning Center"),
  age = c(1, 2, 3), cohort = 5, dose = c(1), mode = "snapshot"
)
canonicalize_target(target, fit_sim)
#>    obs_c_id                   loc_id   age cohort  dose weight loc_c_id
#>       <int>                   <char> <num>  <num> <num>  <num>    <int>
#> 1:        1        Blue Heron School     1      7     1      1        5
#> 2:        2 Bluebird Learning Center     1      7     1      1        6
#> 3:        3        Blue Heron School     2      6     1      1        5
#> 4:        4 Bluebird Learning Center     2      6     1      1        6
#> 5:        5        Blue Heron School     3      5     1      1        5
#> 6:        6 Bluebird Learning Center     3      5     1      1        6
```
