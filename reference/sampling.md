# Immunity: Geographic & Age-based Projection, `imuGAP`

This a sampler interface to convert user-friendly data into the
necessary format to feed the immunity estimation model.

## Usage

``` r
sampling(
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

  - a `positive` column; non-negative integers, the observed number of
    vaccinated individuals

  - a `sample_n` column; positive integers, the number of individuals
    sampled, must be greater than or equal to "positive"

  - optionally, a `censored` column; numeric, NA (uncensored) or 1
    (right-censored); if not present, will be assumed NA

- populations:

  a `[data.frame()]`, the observation meta data, with columns

  - `obs_id`, any type; the observation the row concerns (i.e. id shared
    with an observations data object)

  - `loc_id`, any type; the location the row concerns (i.e. id shared
    with a locations data object)

  - `dose`, a non-zero, positive integer (1, 2, ...); what dose row
    concerns

  - `cohort`, a positive integer; the cohort at the location row
    concerns

  - `age`, a positive integer; the age of that cohort row concerns

  - `weight`, a numeric, (0, 1); the relative contribution of this row
    to an observation. Optional if each population row has a unique
    `obs_id`.

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

An object of class `imugap_fit` wrapping the raw `stanfit` object along
with settings and dataset metadata.

## Examples

``` r
if (FALSE) { # \dontrun{
data("locations_sim"); data("observations_sim"); data("populations_sim")
st_opts <- stan_options(chains = 2, iter = 500)
sampling(
  observations_sim, populations_sim, locations_sim,
  stan_opts = st_opts
)
} # }
```
