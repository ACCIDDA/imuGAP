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

  - an `obs_id` column; any type, as long as unique, non-NA

  - a \`positive“ column; non-negative integers, the observed number of
    vaccinated individuals

  - a `sample_n` column; positive integers, the number of individuals
    sampled, must be greater than or equal to "positive"

  - optionally, a `censored` column; numeric, NA (uncensored) or 1
    (right-censored); if not present, will be assumed NA

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

- locations:

  a `[data.frame()]`, with columns `loc_id` and `parent_id`, of the same
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
