# Assemble Multi-Layer Location Hierarchy Data for Stan

Extracts structural metadata and 1D boundary start indices from a
canonicalized locations table for consumption by Stan multi-layer
models.

## Usage

``` r
assemble_layer_data(loc_info)
```

## Arguments

- loc_info:

  A canonicalized locations table (passed to
  [`canonicalize_locations()`](https://accidda.github.io/imuGAP/reference/canonicalize.md)).

## Value

A named list containing:

- `n_locs`: integer total count of locations

- `n_layers`: integer maximum depth / number of layers (\>= 2)

- `layer_starts`: integer array of starting location indices for each
  layer (length `n_layers`)

- `n_parent_locs`: integer count of parent locations that have children

- `parent_loc_id`: integer array (length `n_parent_locs`) of canonical
  IDs of parent locations

- `parent_child_starts`: integer array (length `n_parent_locs`) of
  starting child location IDs

- `loc_population`: numeric array (length `n_locs`) of population
  weights
