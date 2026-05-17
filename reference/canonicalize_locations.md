# Canonicalize Location Data

Canonicalize Location Data

## Usage

``` r
canonicalize_locations(locations)
```

## Arguments

- locations:

  a `[data.frame()]`, with columns `loc_id` and `parent_id`, of the same
  type. See Details for restrictions.

## Value

a `data.table`, with:

- `loc_id`, `parent_id` columns as originally supplied, possibly
  reordered

- `loc_c_id`, `loc_cp_id` columns, canonicalized id/parent_id columns,
  representing the order that will be used in the sampler

- `layer` column, an integer from 1 (root), 2 (root children), 3
  (grandchildren), &c

- `layer_bound` column, an integer starting from 1 by layer. This
  provides index slice information used in the stan model.

## Details

The `[imuGAP()]` sampler works on a hierarchical model of locations, and
must be provided that structure. This method checks location structure
validity, and returns a canonical version including the layer
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

## Examples

``` r
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
```
