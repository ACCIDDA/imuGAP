# Immunity: Geographic & Age-based Projection, `imuGAP`

This a sampler interface to convert user-friendly data into the
necessary format to feed the immunity estimation model.

## Usage

``` r
imuGAP(
  observations,
  populations,
  locations,
  imugap_opts = imugap_options(),
  stan_opts = stan_options()
)
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

- locations:

  a `[data.frame()]`, with columns `id` and `parent_id`, of the same
  type. See Details for restrictions.

- imugap_opts:

  options for the `imuGAP` model

- stan_opts:

  passed to
  [`rstan::sampling`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
  (e.g. `iter`, `chains`).

## Value

An object of class `stanfit` returned by
[`rstan::sampling`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
