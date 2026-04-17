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

  - "obs_id", the observation id the row concerns

  - "loc_id", the location id the row concerns

  - "dose", which dose the row concerns

  - "cohort", the cohort at that location the row concerns

  - "age", the age of that cohort the row concerns

  - "weight", the relative contribution of this row to an observation
    Note that multiple rows may concern the same observation, meaning
    that the populations from different cohorts, locations, and ages may
    be pooled in an observation

- observations:

  a `[data.frame()]`, the observed data, with at least three columns:

  - an "id" column; any type, as long as unique, non-NA

  - a "positive" column; non-negative integers, the observed number of
    vaccinated individuals

  - a "sample_n" column; positive integers, the number of individuals
    sampled, must be greater than or equal to "positive"

  - optionally, a "censored" column; numeric, NA (uncensored) or 1
    (right-censored);

- locations:

  a `[data.frame()]`, with columns `id` and `parent_id`, of the same
  type. See Details for restrictions.

- max_cohort:

  if present, what is the maximum cohort that should be present?

- max_age:

  if present, what is the maximum age that should be present?

## Value

a canonical populations object, mirroring the input "populations", with
the following updates:

- "obs_c_id", the observation id the row concerns, canonicalized to
  match the canonical observation ids

- "loc_c_id", the location id the row concerns, canonicalized to match

## Details

This method validates the meta-data associated with the observations, as
well as converting that meta-data to use the canonical id format.
