# Canonicalize imuGAP Data Objects

These functions validate, clean, and convert raw user-supplied data
structures (locations, observations, and populations) into the canonical
forms required by the `[sampling()]` sampler and the underlying Stan
models.

## Usage

``` r
canonicalize_locations(locations)

canonicalize_observations(observations, drop_extra = TRUE)

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

- locations:

  a `[data.frame()]`, with columns `loc_id` and `parent_id`, of the same
  type. See Details for restrictions.

- observations:

  a `[data.frame()]`, the observed data, with at least three columns:

  - an `obs_id` column; any type, as long as unique, non-NA

  - a `positive` column; non-negative integers, the observed number of
    vaccinated individuals

  - a `sample_n` column; positive integers, the number of individuals
    sampled, must be greater than or equal to "positive"

  - optionally, a `censored` column; numeric, NA (uncensored) or 1
    (right-censored); if not present, will be assumed NA

- drop_extra:

  a logical scalar; drop extraneous columns? (default: yes)

- populations:

  a `[data.frame()]`, the observation meta data, with columns

  - `obs_id`, any type; the observation the row concerns (i.e. id shared
    with an observations data object)

  - `loc_id`, any type; the location the row concerns (i.e. id shared
    with a locations data object)

  - `dose`, a non-zero, positive integer (1, 2, ...); what dose row
    concerns

  - `cohort`, a positive integer; the cohort at the location row
    concerns

  - `age`, a positive integer; the age of that cohort row concerns

  - `weight`, a numeric, (0, 1); the relative contribution of this row
    to an observation. Optional if each population row has a unique
    `obs_id`.

- max_cohort:

  if present, what is the maximum cohort that should be present?

- max_age:

  if present, what is the maximum age that should be present?

- max_dose:

  maximum dose number to allow (default: 2L)

## Value

`canonicalize_locations` returns a `data.table`, with:

- `loc_id`, `parent_id` columns as originally supplied, possibly
  reordered

- `loc_c_id`, `loc_cp_id` columns, canonicalized id/parent_id columns,
  representing the order that will be used in the sampler

- `layer` column, an integer from 1 (root), 2 (root children), 3
  (grandchildren), &c

- `layer_bound` column, an integer starting from 1 by layer. This
  provides index slice information used in the stan model.

`canonicalize_observations` returns a canonical observation object, a
`[data.table()]` with:

- an `obs_c_id` column, an integer sequence from 1; the order
  observations will be passed to estimation

- the original `obs_id` column, possibly reordered

- `positive` and `sample_n` columns, possibly reordered

- a "censored" column; all NA, if not present in original `observations`
  argument

`canonicalize_populations` returns a canonical populations object,
mirroring the input `populations`, with the following updates:

- `obs_c_id`, the observation id the row concerns, canonicalized to
  match the canonical observation ids

- `loc_c_id`, the location id the row concerns, canonicalized to match

- reordered to `obs_c_id` order

## Details

The `imuGAP` hierarchical modeling framework requires data structures to
adhere to specific relational and format constraints. The three
canonicalize functions process and validate these inputs as described
below:

### Locations (`canonicalize_locations`)

The `[sampling()]` sampler works on a hierarchical model of locations,
and must be provided that structure. This method checks location
structure validity, and returns a canonical version including the layer
membership.

A valid structure has:

- a unique root,

- no cycles, and

- no duplicate `loc_id`s

Users may explicitly identify the root `loc_id` by providing a row with
`parent_id` equal to `NA`. Otherwise, any `parent_id` that does not
appear in `loc_id` is treated as the root.

If the input is valid, this method will create the canonicalized
version. In that version, all ids run from 1:N, where N is the number of
distinct ids. That order is determined by layer order, then position of
parent within its layer, then "natural" order (i.e., whatever base R
[`sort()`](https://rdrr.io/r/base/sort.html) yields).

### Observations (`canonicalize_observations`)

The observations object documents observations used to fit the model.
Conceptually, each row represents an observation of vaccination status
within a population. That population need not be uniform (see
`[canonicalize_populations()]`) or concerning a single cohort or time:
each observation should generally be the best available resolution data.
That resolution can vary across rows. The sampler uses information about
the resolutions to automatically figure out how to compare the latent
process model to those different observations.

For the optional `censored` column: the model supports vaccination
status indicators which are vaccine specific as well as those which
represent an individual having all of a set of vaccines (including the
target vaccine). The specific coverage for the target vaccine is
right-censored in the latter case: full-set-coverage is the minimum
coverage for the target.

When at least some of the data are censored, you must supply the
`censored` column to correctly estimate coverage. Mark any uncensored
observations with `NA`, and any right-censored observations with `1`.
Note that `0` is *not* a valid value at this time; we are preserving
that for potential future support of left-censoring.

### Populations (`canonicalize_populations`)

This method validates the meta-data associated with the observations, as
well as converting that meta-data to use the canonical id formats.

Regarding "cohorts" and "ages": these are counted from 1, by 1 "unit".
You can imagine the units are whatever resolution is appropriate for
your data: months, quarters, years, etc. As long as these are used
consistently, estimation will work, and take on the unit meaning you
used for input.

## Examples

``` r
# --- canonicalize_locations ---
data("locations_sim")
locations_sim
#>                        loc_id population parent_id
#>                        <char>      <num>    <char>
#>  1:                     State 2895.13333      <NA>
#>  2:                   Scruggs 1527.70000     State
#>  3:                    Simone  746.63333     State
#>  4:                    Watson  620.80000     State
#>  5:      Chickadee Elementary  147.83333   Scruggs
#>  6:          Nuthatch Academy  368.53333   Scruggs
#>  7:         Blue Heron School  115.43333   Scruggs
#>  8:     Flycatcher Elementary   59.03333   Scruggs
#>  9:  Bluebird Learning Center   49.63333   Scruggs
#> 10:           Catbird Academy  423.40000   Scruggs
#> 11:          Finch Elementary   32.36667   Scruggs
#> 12:            Sparrow School   86.93333   Scruggs
#> 13: Towhee Children's Academy  207.13333   Scruggs
#> 14:        Warbler Elementary   37.40000   Scruggs
#> 15:          Egret Elementary  165.43333    Simone
#> 16:          Cardinal Academy   27.96667    Simone
#> 17:            Bunting School   93.70000    Simone
#> 18:           Tanager Academy   34.23333    Simone
#> 19:      Oriole Youth Academy  166.23333    Simone
#> 20:  Grosbeak Learning Center   27.73333    Simone
#> 21:          Junco Elementary  231.33333    Simone
#> 22:         Meadowlark School   52.66667    Watson
#> 23:      Goldfinch Elementary  110.50000    Watson
#> 24:       Mockingbird Academy   81.00000    Watson
#> 25:   Kinglet Learning Center   87.76667    Watson
#> 26:              Vireo School   58.23333    Watson
#> 27:        Kingfisher Academy   58.43333    Watson
#> 28:      Cormorant Elementary  172.20000    Watson
#>                        loc_id population parent_id
#>                        <char>      <num>    <char>
canonicalize_locations(locations_sim)
#> Key: <layer, parent_id, loc_id>
#>                        loc_id population parent_id layer loc_c_id loc_cp_id
#>                        <char>      <num>    <char> <int>    <int>     <int>
#>  1:                     State 2895.13333      <NA>     1        1        NA
#>  2:                   Scruggs 1527.70000     State     2        2         1
#>  3:                    Simone  746.63333     State     2        3         1
#>  4:                    Watson  620.80000     State     2        4         1
#>  5:         Blue Heron School  115.43333   Scruggs     3        5         2
#>  6:  Bluebird Learning Center   49.63333   Scruggs     3        6         2
#>  7:           Catbird Academy  423.40000   Scruggs     3        7         2
#>  8:      Chickadee Elementary  147.83333   Scruggs     3        8         2
#>  9:          Finch Elementary   32.36667   Scruggs     3        9         2
#> 10:     Flycatcher Elementary   59.03333   Scruggs     3       10         2
#> 11:          Nuthatch Academy  368.53333   Scruggs     3       11         2
#> 12:            Sparrow School   86.93333   Scruggs     3       12         2
#> 13: Towhee Children's Academy  207.13333   Scruggs     3       13         2
#> 14:        Warbler Elementary   37.40000   Scruggs     3       14         2
#> 15:            Bunting School   93.70000    Simone     3       15         3
#> 16:          Cardinal Academy   27.96667    Simone     3       16         3
#> 17:          Egret Elementary  165.43333    Simone     3       17         3
#> 18:  Grosbeak Learning Center   27.73333    Simone     3       18         3
#> 19:          Junco Elementary  231.33333    Simone     3       19         3
#> 20:      Oriole Youth Academy  166.23333    Simone     3       20         3
#> 21:           Tanager Academy   34.23333    Simone     3       21         3
#> 22:      Cormorant Elementary  172.20000    Watson     3       22         4
#> 23:      Goldfinch Elementary  110.50000    Watson     3       23         4
#> 24:        Kingfisher Academy   58.43333    Watson     3       24         4
#> 25:   Kinglet Learning Center   87.76667    Watson     3       25         4
#> 26:         Meadowlark School   52.66667    Watson     3       26         4
#> 27:       Mockingbird Academy   81.00000    Watson     3       27         4
#> 28:              Vireo School   58.23333    Watson     3       28         4
#>                        loc_id population parent_id layer loc_c_id loc_cp_id
#>                        <char>      <num>    <char> <int>    <int>     <int>
#>     layer_bound
#>           <int>
#>  1:           1
#>  2:           1
#>  3:           1
#>  4:           1
#>  5:           1
#>  6:           1
#>  7:           1
#>  8:           1
#>  9:           1
#> 10:           1
#> 11:           1
#> 12:           1
#> 13:           1
#> 14:           1
#> 15:          11
#> 16:          11
#> 17:          11
#> 18:          11
#> 19:          11
#> 20:          11
#> 21:          11
#> 22:          18
#> 23:          18
#> 24:          18
#> 25:          18
#> 26:          18
#> 27:          18
#> 28:          18
#>     layer_bound
#>           <int>
# can also be provided in non-canonical order, and with an implicit root
weird_locations <- subset(locations_sim, !is.na(parent_id))[
  sample(nrow(locations_sim) - 1L)
]
canonicalize_locations(weird_locations)
#> Key: <layer, parent_id, loc_id>
#>                        loc_id population parent_id layer loc_c_id loc_cp_id
#>                        <char>      <num>    <char> <int>    <int>     <int>
#>  1:                     State 2895.13333      <NA>     1        1        NA
#>  2:                   Scruggs 1527.70000     State     2        2         1
#>  3:                    Simone  746.63333     State     2        3         1
#>  4:                    Watson  620.80000     State     2        4         1
#>  5:         Blue Heron School  115.43333   Scruggs     3        5         2
#>  6:  Bluebird Learning Center   49.63333   Scruggs     3        6         2
#>  7:           Catbird Academy  423.40000   Scruggs     3        7         2
#>  8:      Chickadee Elementary  147.83333   Scruggs     3        8         2
#>  9:          Finch Elementary   32.36667   Scruggs     3        9         2
#> 10:     Flycatcher Elementary   59.03333   Scruggs     3       10         2
#> 11:          Nuthatch Academy  368.53333   Scruggs     3       11         2
#> 12:            Sparrow School   86.93333   Scruggs     3       12         2
#> 13: Towhee Children's Academy  207.13333   Scruggs     3       13         2
#> 14:        Warbler Elementary   37.40000   Scruggs     3       14         2
#> 15:            Bunting School   93.70000    Simone     3       15         3
#> 16:          Cardinal Academy   27.96667    Simone     3       16         3
#> 17:          Egret Elementary  165.43333    Simone     3       17         3
#> 18:  Grosbeak Learning Center   27.73333    Simone     3       18         3
#> 19:          Junco Elementary  231.33333    Simone     3       19         3
#> 20:      Oriole Youth Academy  166.23333    Simone     3       20         3
#> 21:           Tanager Academy   34.23333    Simone     3       21         3
#> 22:      Cormorant Elementary  172.20000    Watson     3       22         4
#> 23:      Goldfinch Elementary  110.50000    Watson     3       23         4
#> 24:        Kingfisher Academy   58.43333    Watson     3       24         4
#> 25:   Kinglet Learning Center   87.76667    Watson     3       25         4
#> 26:         Meadowlark School   52.66667    Watson     3       26         4
#> 27:       Mockingbird Academy   81.00000    Watson     3       27         4
#> 28:              Vireo School   58.23333    Watson     3       28         4
#>                        loc_id population parent_id layer loc_c_id loc_cp_id
#>                        <char>      <num>    <char> <int>    <int>     <int>
#>     layer_bound
#>           <int>
#>  1:           1
#>  2:           1
#>  3:           1
#>  4:           1
#>  5:           1
#>  6:           1
#>  7:           1
#>  8:           1
#>  9:           1
#> 10:           1
#> 11:           1
#> 12:           1
#> 13:           1
#> 14:           1
#> 15:          11
#> 16:          11
#> 17:          11
#> 18:          11
#> 19:          11
#> 20:          11
#> 21:          11
#> 22:          18
#> 23:          18
#> 24:          18
#> 25:          18
#> 26:          18
#> 27:          18
#> 28:          18
#>     layer_bound
#>           <int>
# --- canonicalize_observations ---
data("observations_sim")
observations_sim
#>      cohort parent_id               loc_id positive sample_n age_min  dose
#>       <int>    <char>               <char>    <num>    <num>   <int> <int>
#>   1:      1   Scruggs Chickadee Elementary      111      155       5     2
#>   2:      2   Scruggs Chickadee Elementary       99      152       5     2
#>   3:      3   Scruggs Chickadee Elementary      110      156       5     2
#>   4:      4   Scruggs Chickadee Elementary      104      155       5     2
#>   5:      5   Scruggs Chickadee Elementary      123      155       5     2
#>  ---                                                                      
#> 837:     18     State               Watson      160      184      11     2
#> 838:     19     State               Watson      195      220      11     2
#> 839:     20     State               Watson      142      174      11     2
#> 840:     21     State               Watson      207      228      11     2
#> 841:     22     State               Watson      171      201      11     2
#>      censored age_max obs_id cohort_min
#>         <num>   <int>  <int>      <int>
#>   1:       NA      NA      1          1
#>   2:       NA      NA      2          2
#>   3:       NA      NA      3          3
#>   4:       NA      NA      4          4
#>   5:       NA      NA      5          5
#>  ---                                   
#> 837:        1      NA    837         18
#> 838:        1      NA    838         19
#> 839:        1      NA    839         20
#> 840:        1      NA    840         21
#> 841:        1      NA    841         22
canonicalize_observations(observations_sim)
#> Key: <censored, obs_id>
#>      obs_c_id positive sample_n censored obs_id
#>         <int>    <int>    <int>    <num>  <int>
#>   1:        1      111      155       NA      1
#>   2:        2       99      152       NA      2
#>   3:        3      110      156       NA      3
#>   4:        4      104      155       NA      4
#>   5:        5      123      155       NA      5
#>  ---                                           
#> 837:      837      160      184        1    837
#> 838:      838      195      220        1    838
#> 839:      839      142      174        1    839
#> 840:      840      207      228        1    840
#> 841:      841      171      201        1    841
# --- canonicalize_populations ---
data("populations_sim"); data("locations_sim"); data("observations_sim")
populations_sim
#>      obs_id               loc_id cohort   age  dose weight
#>       <int>               <char>  <int> <int> <int>  <num>
#>   1:      1 Chickadee Elementary      1     5     2      1
#>   2:      2 Chickadee Elementary      2     5     2      1
#>   3:      3 Chickadee Elementary      3     5     2      1
#>   4:      4 Chickadee Elementary      4     5     2      1
#>   5:      5 Chickadee Elementary      5     5     2      1
#>  ---                                                      
#> 897:    837               Watson     18    11     2      1
#> 898:    838               Watson     19    11     2      1
#> 899:    839               Watson     20    11     2      1
#> 900:    840               Watson     21    11     2      1
#> 901:    841               Watson     22    11     2      1
canonicalize_populations(populations_sim, observations_sim, locations_sim)
#> Key: <obs_c_id, loc_c_id, cohort, age, dose>
#>      obs_id               loc_id cohort   age  dose weight obs_c_id loc_c_id
#>       <int>               <char>  <int> <int> <int>  <num>    <int>    <int>
#>   1:      1 Chickadee Elementary      1     5     2      1        1        8
#>   2:      2 Chickadee Elementary      2     5     2      1        2        8
#>   3:      3 Chickadee Elementary      3     5     2      1        3        8
#>   4:      4 Chickadee Elementary      4     5     2      1        4        8
#>   5:      5 Chickadee Elementary      5     5     2      1        5        8
#>  ---                                                                        
#> 897:    837               Watson     18    11     2      1      837        4
#> 898:    838               Watson     19    11     2      1      838        4
#> 899:    839               Watson     20    11     2      1      839        4
#> 900:    840               Watson     21    11     2      1      840        4
#> 901:    841               Watson     22    11     2      1      841        4
#>      range_start
#>            <int>
#>   1:           1
#>   2:           2
#>   3:           3
#>   4:           4
#>   5:           5
#>  ---            
#> 897:         897
#> 898:         898
#> 899:         899
#> 900:         900
#> 901:         901
```
