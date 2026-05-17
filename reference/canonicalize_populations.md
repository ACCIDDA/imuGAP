# Check observation meta data object

Check observation meta data object

## Usage

``` r
canonicalize_populations(
  populations,
  observations,
  locations,
  max_cohort,
  max_age,
  max_dose = 2L
)
```

## Arguments

- populations:

  a `[data.frame()]`, the the observation meta data, with columns

  - `obs_id`, any type; the observation the row concerns (i.e. id shared
    with an observations data object)

  - \`loc_id“, any type; the location the row concerns (i.e. id shared
    with a locations data object)

  - `dose`, a non-zero, positive integer (1, 2, ...); what dose row
    concerns

  - `cohort`, a positive integer; the cohort at the location row
    concerns

  - `age`, a positive integer; the age of that cohort row concerns

  - `weight`, a numeric, (0, 1); the relative contribution of this row
    to an observation Note that multiple rows may concern the same
    observation, meaning that the populations from different cohorts,
    locations, and ages may be pooled in an observation

- observations:

  a `[data.frame()]`, the observed data, with at least three columns:

  - an `obs_id` column; any type, as long as unique, non-NA

  - a \`positive“ column; non-negative integers, the observed number of
    vaccinated individuals

  - a `sample_n` column; positive integers, the number of individuals
    sampled, must be greater than or equal to "positive"

  - optionally, a `censored` column; numeric, NA (uncensored) or 1
    (right-censored); if not present, will be assumed NA

- locations:

  a `[data.frame()]`, with columns `loc_id` and `parent_id`, of the same
  type. See Details for restrictions.

- max_cohort:

  if present, what is the maximum cohort that should be present?

- max_age:

  if present, what is the maximum age that should be present?

- max_dose:

  maximum dose number to allow (default: 2L)

## Value

a canonical populations object, mirroring the input `populations`, with
the following updates:

- `obs_c_id`, the observation id the row concerns, canonicalized to
  match the canonical observation ids

- `loc_c_id`, the location id the row concerns, canonicalized to match

- reordered to `obs_c_id` order

## Details

This method validates the meta-data associated with the observations, as
well as converting that meta-data to use the canonical id formats.

Regarding "cohorts" and "ages": these are counted from 1, by 1 "unit".
You can imagine the units are whatever resolution is appropriate for
your data: months, quarters, years, etc. As long as these are used
consistently, estimation will work, and take on the unit meaning you
used for input.

## Examples

``` r
data("populations_sim"); data("locations_sim"); data("observations_sim")
populations_sim
#>      obs_id               loc_id cohort   age  dose weight
#>       <num>               <char>  <num> <num> <num>  <num>
#>   1:      1 Chickadee Elementary      4     5     2    1.0
#>   2:      2 Chickadee Elementary      5     5     2    1.0
#>   3:      3 Chickadee Elementary      6     5     2    1.0
#>   4:      4 Chickadee Elementary      7     5     2    1.0
#>   5:      5 Chickadee Elementary      8     5     2    1.0
#>  ---                                                      
#> 746:    698                State     19    14     2    0.2
#> 747:    698                State     18    15     2    0.2
#> 748:    698                State     17    16     2    0.2
#> 749:    698                State     16    17     2    0.2
#> 750:    698                State     15    18     2    0.2
canonicalize_populations(populations_sim, observations_sim, locations_sim)
#> Key: <obs_c_id, loc_c_id, cohort, age, dose>
#>      obs_id               loc_id cohort   age  dose weight obs_c_id loc_c_id
#>       <num>               <char>  <int> <int> <num>  <num>    <int>    <int>
#>   1:      1 Chickadee Elementary      4     5     2      1        1        8
#>   2:      2 Chickadee Elementary      5     5     2      1        2        8
#>   3:      3 Chickadee Elementary      6     5     2      1        3        8
#>   4:      4 Chickadee Elementary      7     5     2      1        4        8
#>   5:      5 Chickadee Elementary      8     5     2      1        5        8
#>  ---                                                                        
#> 746:    562   Kingfisher Academy     15     5     2      1      694       24
#> 747:    581 Cormorant Elementary      9     5     2      1      695       22
#> 748:    582 Cormorant Elementary     10     5     2      1      696       22
#> 749:    587 Cormorant Elementary     15     5     2      1      697       22
#> 750:    592 Cormorant Elementary     20     5     2      1      698       22
#>      range_start
#>            <int>
#>   1:           1
#>   2:           2
#>   3:           3
#>   4:           4
#>   5:           5
#>  ---            
#> 746:         746
#> 747:         747
#> 748:         748
#> 749:         749
#> 750:         750
```
