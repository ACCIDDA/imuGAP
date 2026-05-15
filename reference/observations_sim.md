# Example Observation Data

A dataset containing vaccine coverage observations.

## Usage

``` r
observations_sim
```

## Format

A `[data.table()]` with 698 rows and 4+ columns:

- `obs_id`, a number, the observation id (primary key)

- `positive`, a number, how many individuals had the vaccine

- `sample_n`, a number, the number of individuals in the observations

- `censored`, a number, 1 if `positive` is right censored and `NA` if
  uncensored

- ... assorted other columns that are irrelevant to use as observations
  arg
