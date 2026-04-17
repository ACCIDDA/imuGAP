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

  - `dose`, a non-zero, positive integer (1, 2, ...); which dose the row
    concerns

  - `cohort`, a positive integer; the cohort at that location the row
    concerns

  - `age`, a positive integer; the age of that cohort the row concerns

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
