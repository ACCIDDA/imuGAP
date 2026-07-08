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
#>                        loc_id parent_id
#>                        <char>    <char>
#>  1:                     State      <NA>
#>  2:                   Scruggs     State
#>  3:                    Simone     State
#>  4:                    Watson     State
#>  5:      Chickadee Elementary   Scruggs
#>  6:          Nuthatch Academy   Scruggs
#>  7:         Blue Heron School   Scruggs
#>  8:     Flycatcher Elementary   Scruggs
#>  9:  Bluebird Learning Center   Scruggs
#> 10:           Catbird Academy   Scruggs
#> 11:          Finch Elementary   Scruggs
#> 12:            Sparrow School   Scruggs
#> 13: Towhee Children's Academy   Scruggs
#> 14:        Warbler Elementary   Scruggs
#> 15:          Egret Elementary    Simone
#> 16:          Cardinal Academy    Simone
#> 17:            Bunting School    Simone
#> 18:           Tanager Academy    Simone
#> 19:      Oriole Youth Academy    Simone
#> 20:  Grosbeak Learning Center    Simone
#> 21:          Junco Elementary    Simone
#> 22:         Meadowlark School    Watson
#> 23:      Goldfinch Elementary    Watson
#> 24:       Mockingbird Academy    Watson
#> 25:   Kinglet Learning Center    Watson
#> 26:              Vireo School    Watson
#> 27:        Kingfisher Academy    Watson
#> 28:      Cormorant Elementary    Watson
#>                        loc_id parent_id
#>                        <char>    <char>
canonicalize_locations(locations_sim)
#> Key: <layer, parent_id, loc_id>
#>                        loc_id parent_id layer loc_c_id loc_cp_id layer_bound
#>                        <char>    <char> <int>    <int>     <int>       <int>
#>  1:                     State      <NA>     1        1        NA           1
#>  2:                   Scruggs     State     2        2         1           1
#>  3:                    Simone     State     2        3         1           1
#>  4:                    Watson     State     2        4         1           1
#>  5:         Blue Heron School   Scruggs     3        5         2           1
#>  6:  Bluebird Learning Center   Scruggs     3        6         2           1
#>  7:           Catbird Academy   Scruggs     3        7         2           1
#>  8:      Chickadee Elementary   Scruggs     3        8         2           1
#>  9:          Finch Elementary   Scruggs     3        9         2           1
#> 10:     Flycatcher Elementary   Scruggs     3       10         2           1
#> 11:          Nuthatch Academy   Scruggs     3       11         2           1
#> 12:            Sparrow School   Scruggs     3       12         2           1
#> 13: Towhee Children's Academy   Scruggs     3       13         2           1
#> 14:        Warbler Elementary   Scruggs     3       14         2           1
#> 15:            Bunting School    Simone     3       15         3          11
#> 16:          Cardinal Academy    Simone     3       16         3          11
#> 17:          Egret Elementary    Simone     3       17         3          11
#> 18:  Grosbeak Learning Center    Simone     3       18         3          11
#> 19:          Junco Elementary    Simone     3       19         3          11
#> 20:      Oriole Youth Academy    Simone     3       20         3          11
#> 21:           Tanager Academy    Simone     3       21         3          11
#> 22:      Cormorant Elementary    Watson     3       22         4          18
#> 23:      Goldfinch Elementary    Watson     3       23         4          18
#> 24:        Kingfisher Academy    Watson     3       24         4          18
#> 25:   Kinglet Learning Center    Watson     3       25         4          18
#> 26:         Meadowlark School    Watson     3       26         4          18
#> 27:       Mockingbird Academy    Watson     3       27         4          18
#> 28:              Vireo School    Watson     3       28         4          18
#>                        loc_id parent_id layer loc_c_id loc_cp_id layer_bound
#>                        <char>    <char> <int>    <int>     <int>       <int>
# can also be provided in non-canonical order, and with an implicit root
weird_locations <- subset(locations_sim, !is.na(parent_id))[
  sample(nrow(locations_sim) - 1L)
]
canonicalize_locations(weird_locations)
#> Key: <layer, parent_id, loc_id>
#>                        loc_id parent_id layer loc_c_id loc_cp_id layer_bound
#>                        <char>    <char> <int>    <int>     <int>       <int>
#>  1:                     State      <NA>     1        1        NA           1
#>  2:                   Scruggs     State     2        2         1           1
#>  3:                    Simone     State     2        3         1           1
#>  4:                    Watson     State     2        4         1           1
#>  5:         Blue Heron School   Scruggs     3        5         2           1
#>  6:  Bluebird Learning Center   Scruggs     3        6         2           1
#>  7:           Catbird Academy   Scruggs     3        7         2           1
#>  8:      Chickadee Elementary   Scruggs     3        8         2           1
#>  9:          Finch Elementary   Scruggs     3        9         2           1
#> 10:     Flycatcher Elementary   Scruggs     3       10         2           1
#> 11:          Nuthatch Academy   Scruggs     3       11         2           1
#> 12:            Sparrow School   Scruggs     3       12         2           1
#> 13: Towhee Children's Academy   Scruggs     3       13         2           1
#> 14:        Warbler Elementary   Scruggs     3       14         2           1
#> 15:            Bunting School    Simone     3       15         3          11
#> 16:          Cardinal Academy    Simone     3       16         3          11
#> 17:          Egret Elementary    Simone     3       17         3          11
#> 18:  Grosbeak Learning Center    Simone     3       18         3          11
#> 19:          Junco Elementary    Simone     3       19         3          11
#> 20:      Oriole Youth Academy    Simone     3       20         3          11
#> 21:           Tanager Academy    Simone     3       21         3          11
#> 22:      Cormorant Elementary    Watson     3       22         4          18
#> 23:      Goldfinch Elementary    Watson     3       23         4          18
#> 24:        Kingfisher Academy    Watson     3       24         4          18
#> 25:   Kinglet Learning Center    Watson     3       25         4          18
#> 26:         Meadowlark School    Watson     3       26         4          18
#> 27:       Mockingbird Academy    Watson     3       27         4          18
#> 28:              Vireo School    Watson     3       28         4          18
#>                        loc_id parent_id layer loc_c_id loc_cp_id layer_bound
#>                        <char>    <char> <int>    <int>     <int>       <int>
# --- canonicalize_observations ---
data("observations_sim")
observations_sim
#>       year parent_id               loc_id positive sample_n age_min  dose
#>      <num>    <char>               <char>    <num>    <num>   <num> <int>
#>   1:  2001   Scruggs Chickadee Elementary       42       52       5     2
#>   2:  2002   Scruggs Chickadee Elementary       41       51       5     2
#>   3:  2003   Scruggs Chickadee Elementary       46       50       5     2
#>   4:  2004   Scruggs Chickadee Elementary       41       48       5     2
#>   5:  2005   Scruggs Chickadee Elementary       44       53       5     2
#>  ---                                                                     
#> 694:  2021      <NA>                State      254      281      14     2
#> 695:  2022      <NA>                State      252      274      14     2
#> 696:  2023      <NA>                State      230      251      14     2
#> 697:  2024      <NA>                State      272      296      14     2
#> 698:  2025      <NA>                State      210      230      14     2
#>      censored age_max cohort_min obs_id
#>         <num>   <int>      <num>  <int>
#>   1:       NA      NA          4      1
#>   2:       NA      NA          5      2
#>   3:       NA      NA          6      3
#>   4:       NA      NA          7      4
#>   5:       NA      NA          8      5
#>  ---                                   
#> 694:       NA      19         11    694
#> 695:       NA      19         12    695
#> 696:       NA      19         13    696
#> 697:       NA      19         14    697
#> 698:       NA      19         15    698
canonicalize_observations(observations_sim)
#> Key: <censored, obs_id>
#>      obs_c_id positive sample_n censored obs_id
#>         <int>    <int>    <int>    <num>  <int>
#>   1:        1       42       52       NA      1
#>   2:        2       41       51       NA      2
#>   3:        3       46       50       NA      3
#>   4:        4       41       48       NA      4
#>   5:        5       44       53       NA      5
#>  ---                                           
#> 694:      694      242      276        1    656
#> 695:      695      247      297        1    657
#> 696:      696      220      276        1    658
#> 697:      697      267      328        1    659
#> 698:      698      315      374        1    660
# --- canonicalize_populations ---
data("populations_sim"); data("locations_sim"); data("observations_sim")
populations_sim
#>      obs_id               loc_id cohort   age  dose weight
#>       <int>               <char>  <int> <int> <int>  <num>
#>   1:      1 Chickadee Elementary      4     5     2      1
#>   2:      2 Chickadee Elementary      5     5     2      1
#>   3:      3 Chickadee Elementary      6     5     2      1
#>   4:      4 Chickadee Elementary      7     5     2      1
#>   5:      5 Chickadee Elementary      8     5     2      1
#>  ---                                                      
#> 746:    656                State     26     3     1      1
#> 747:    657                State     27     3     1      1
#> 748:    658                State     28     3     1      1
#> 749:    659                State     29     3     1      1
#> 750:    660                State     30     3     1      1
canonicalize_populations(populations_sim, observations_sim, locations_sim)
#> Key: <obs_c_id, loc_c_id, cohort, age, dose>
#>      obs_id               loc_id cohort   age  dose weight obs_c_id loc_c_id
#>       <int>               <char>  <int> <int> <int>  <num>    <int>    <int>
#>   1:      1 Chickadee Elementary      4     5     2      1        1        8
#>   2:      2 Chickadee Elementary      5     5     2      1        2        8
#>   3:      3 Chickadee Elementary      6     5     2      1        3        8
#>   4:      4 Chickadee Elementary      7     5     2      1        4        8
#>   5:      5 Chickadee Elementary      8     5     2      1        5        8
#>  ---                                                                        
#> 746:    656                State     26     3     1      1      694        1
#> 747:    657                State     27     3     1      1      695        1
#> 748:    658                State     28     3     1      1      696        1
#> 749:    659                State     29     3     1      1      697        1
#> 750:    660                State     30     3     1      1      698        1
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
