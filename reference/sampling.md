# Immunity: Geographic & Age-based Projection, `imuGAP`

Fits the imuGAP Bayesian hierarchical vaccine coverage estimation model
across arbitrary user-specified location partitions, birth cohorts,
ages, and vaccine doses.

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

  options for the `imuGAP` model, created by `[imugap_options()]`.

- stan_opts:

  sampler configuration created by `[stan_options()]` (see
  `[flexstanr::stan_options()]` for details on supported sampler
  arguments, including `iter`, `chains`, `cores`, `seed`, and
  `backend`).

## Value

An object of class `imugap_fit` wrapping the raw `stanfit` (or
`CmdStanMCMC`) object along with model settings and dataset metadata.

## Details

`sampling()` automatically inspects the depth of the location hierarchy
supplied in `locations` via `[canonicalize_locations()]` and
`[assemble_layer_data()]`:

- **Single-layer (1 layer)**: When only a root location is supplied,
  `sampling()` automatically dispatches to the optimized single-location
  model.

- **Multi-layer (\>= 2 layers)**: When hierarchical sub-locations are
  supplied (e.g., 2-layer state -\> county, 3-layer state -\> county -\>
  school, or deeper trees), `sampling()` dispatches to the general
  hierarchical model with partial pooling across layer-specific variance
  components.

If the Stan sampler fails to initialize and produces no draws (for the
rstan backend, a mode-2 `stanfit` with an empty `@sim`), `sampling()`
raises an error of class `imugap_no_draws` rather than returning an
empty fit, so the failure can be handled with
[`tryCatch()`](https://rdrr.io/r/base/conditions.html). The check is
backend-agnostic (see `backend_has_draws()`).

## Examples

``` r
if (FALSE) { # interactive()
# \donttest{
data("locations_sim")
data("observations_sim")
data("populations_sim")
st_opts <- stan_options(chains = 2, iter = 500)
sampling(
  observations_sim, populations_sim, locations_sim,
  stan_opts = st_opts
)
# }
}
```
