# Canonicalize Observation Data

Canonicalize Observation Data

## Usage

``` r
canonicalize_observations(observations)
```

## Arguments

- observations:

  a `[data.frame()]`, the observed data, with at least three columns:

  - an "id" column; any type, as long as unique, non-NA

  - a "positive" column; non-negative integers, the observed number of
    vaccinated individuals

  - a "sample_n" column; positive integers, the number of individuals
    sampled, must be greater than or equal to "positive"

  - optionally, a "censored" column; numeric, NA (uncensored) or 1
    (right-censored);

## Value

a canonical observation object, a `[data.table()]` with:

- an "c_id" column, an integer sequence from 1; the order observations
  will be passed to estimation

- the original "id" column, possibly reordered

- "positive" and "sample_n" columns, possibly reordered

- a "censored" column; all NA, if not present in original `observations`
  argument

## Details

The observations object documents observations used to fit the model.
Conceptually, each row represents an observation of vaccination status
within a population. That population need not be uniform (see
`[canonicalize_populations()]`) or concerning a single cohort or time:
each observation should generally be the best available resolution data.
That resolution can vary across rows. The `[imuGAP()]` sampler uses
information about the resolutions to automatically figure out how to
compare the latent process model to those different observations.

For the optional "censored" column: the model supports vaccination
status indicators which are vaccine specific as well as those which
represent an individual having all of a set of vaccines (including the
target vaccine). The specific coverage for the target vaccine is
right-censored in the latter case: the all-coverage is the minimum
coverage for the target.

When at least some of the data are censored, you must supply the
"censored" column to correctly estimate coverage. Mark any uncensored
observations with NA, and any right-censored observations with 1. Note
that "0" is *not* a valid value at this time; we are preserving that for
potential future support of left-censoring.
