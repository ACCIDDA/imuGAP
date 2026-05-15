# Example Population Data

A dataset containing the meta-data about vaccine coverage observations.

## Usage

``` r
populations_sim
```

## Format

A `[data.table()]` with 750 rows and 6 columns:

- `obs_id`, a number, the observation (foreign key to observations)

- `loc_id`, a string, the location (foreign key to locations)

- `cohort`, a number, the birth cohort

- `age`, a number, the age of cohort at time of observation

- `dose`, a number, which dose the observation concerns

- `weight`, a number (0-1), fraction of the observation this row
  represents
