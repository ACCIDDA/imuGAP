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
