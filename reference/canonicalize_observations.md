# Canonicalize Observation Data

Canonicalize Observation Data

## Usage

``` r
canonicalize_observations(observations, drop_extra = TRUE)
```

## Arguments

- observations:

  a `[data.frame()]`, the observed data, with at least three columns:

  - an `obs_id` column; any type, as long as unique, non-NA

  - a \`positive“ column; non-negative integers, the observed number of
    vaccinated individuals

  - a `sample_n` column; positive integers, the number of individuals
    sampled, must be greater than or equal to "positive"

  - optionally, a `censored` column; numeric, NA (uncensored) or 1
    (right-censored); if not present, will be assumed NA

- drop_extra:

  a logical scalar; drop extraneous columns? (default: yes)

## Value

a canonical observation object, a `[data.table()]` with:

- an `obs_c_id` column, an integer sequence from 1; the order
  observations will be passed to estimation

- the original `obs_id` column, possibly reordered

- `positive` and `sample_n` columns, possibly reordered

- a "censored" column; all NA, if not present in original `observations`
  argument

## Details

The observations object documents observations used to fit the model.
Conceptually, each row represents an observation of vaccination status
within a population. That population need not be uniform (see
`[canonicalize_populations()]`) or concerning a single cohort or time:
each observation should generally be the best available resolution data.
That resolution can vary across rows. The `[imuGAP()]` sampler uses
information about the resolutions to automatically figure out how to
compare the latent process model to those different observations.

For the optional `censored` column: the model supports vaccination
status indicators which are vaccine specific as well as those which
represent an individual having all of a set of vaccines (including the
target vaccine). The specific coverage for the target vaccine is
right-censored in the latter case: full-set-coverage is the minimum
coverage for the target.

When at least some of the data are censored, you must supply the
`censored` column to correctly estimate coverage. Mark any uncensored
observations with NA, and any right-censored observations with \$1\$.
Note that \$0\$ is *not* a valid value at this time; we are preserving
that for potential future support of left-censoring.

## Examples

``` r
data("observations_sim")
observations_sim
#>                    loc_id parent_id  year enc_unit_id unit_id positive sample_n
#>                    <char>    <char> <num>       <num>   <num>    <num>    <num>
#>   1: Chickadee Elementary   Scruggs  2001           2       5       61       78
#>   2: Chickadee Elementary   Scruggs  2002           2       5       72       79
#>   3: Chickadee Elementary   Scruggs  2003           2       5       69       81
#>   4: Chickadee Elementary   Scruggs  2004           2       5       68       82
#>   5: Chickadee Elementary   Scruggs  2005           2       5       72       80
#>  ---                                                                           
#> 694:                State      <NA>  2021          NA       1      248      285
#> 695:                State      <NA>  2022          NA       1      200      220
#> 696:                State      <NA>  2023          NA       1      259      280
#> 697:                State      <NA>  2024          NA       1      267      290
#> 698:                State      <NA>  2025          NA       1      257      275
#>      censored ly_min ly_max  dose weight vaxview_type    age cohort_min
#>         <num>  <num>  <num> <num>  <num>       <char> <char>      <num>
#>   1:       NA      5      5     2    1.0         <NA>   <NA>          4
#>   2:       NA      5      5     2    1.0         <NA>   <NA>          5
#>   3:       NA      5      5     2    1.0         <NA>   <NA>          6
#>   4:       NA      5      5     2    1.0         <NA>   <NA>          7
#>   5:       NA      5      5     2    1.0         <NA>   <NA>          8
#>  ---                                                                   
#> 694:       NA     14     18     2    0.2         teen   <NA>         11
#> 695:       NA     14     18     2    0.2         teen   <NA>         12
#> 696:       NA     14     18     2    0.2         teen   <NA>         13
#> 697:       NA     14     18     2    0.2         teen   <NA>         14
#> 698:       NA     14     18     2    0.2         teen   <NA>         15
#>      cohort_max obs_id
#>           <num>  <int>
#>   1:          4      1
#>   2:          5      2
#>   3:          6      3
#>   4:          7      4
#>   5:          8      5
#>  ---                  
#> 694:         15    694
#> 695:         16    695
#> 696:         17    696
#> 697:         18    697
#> 698:         19    698
canonicalize_observations(observations_sim)
#> Key: <censored, obs_id>
#>      obs_c_id positive sample_n censored obs_id
#>         <int>    <int>    <int>    <num>  <int>
#>   1:        1       61       78       NA      1
#>   2:        2       72       79       NA      2
#>   3:        3       69       81       NA      3
#>   4:        4       68       82       NA      4
#>   5:        5       72       80       NA      5
#>  ---                                           
#> 694:      694      245      277        1    562
#> 695:      695       66       74        1    581
#> 696:      696       71       76        1    582
#> 697:      697       70       81        1    587
#> 698:      698       76       81        1    592
```
