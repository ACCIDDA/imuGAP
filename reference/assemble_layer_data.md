# Assemble Location Hierarchy Data for Stan Model

Extracts structural metadata and indexing maps from a canonicalized
locations table for consumption by the Stan model.

## Usage

``` r
assemble_layer_data(loc_info)
```

## Arguments

- loc_info:

  A canonicalized locations table (or raw locations table passed to
  [`canonicalize_locations()`](https://accidda.github.io/imuGAP/reference/canonicalize.md)).

## Value

A named list containing:

- `n_locs`: integer total count of locations

- `n_layers`: integer maximum depth / number of layers

- `layer_sizes`: integer array of location counts per layer (length
  `n_layers`)

- `layer_bounds`: 2 x `n_layers` integer matrix with start/end indices
  for each layer

- `parent_id_map`: integer array (length `n_locs`) mapping each location
  to its parent `loc_c_id` (0 for root)

- `layer_id_map`: integer array (length `n_locs`) mapping each location
  to its layer (1..n_layers)

- `n_parent_locs`: integer count of locations that have children (0 for
  1-layer)

- `parent_loc_id`: integer array (length `n_parent_locs`) of canonical
  IDs of parent locations

- `parent_child_bounds`: 2 x `n_parent_locs` integer matrix with
  start/end child indices for each parent
