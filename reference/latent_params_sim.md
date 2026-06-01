# Example Latent Parameter Values

A list containing the true/latent parameter values used to simulate the
example datasets (`locations_sim`, `populations_sim`,
`observations_sim`).

## Usage

``` r
latent_params_sim
```

## Format

A list with 7 components:

- `phi_state`, a numeric vector of length 30 representing the
  state-specific baseline vaccine uptake propensity over cohorts.

- `lambda`, a numeric vector of length 2 representing the rate
  parameters for vaccine doses 1 and 2 respectively.

- `sigma_sch`, a number, the standard deviation of school-level random
  effects.

- `sigma_cnty`, a number, the standard deviation of county-level random
  effects.

- `off_sch`, a numeric vector of length 24 containing school-level
  random offsets.

- `off_cnty`, a numeric vector of length 3 containing county-level
  random offsets.

- `censor_reduction`, a number representing the censoring offset
  multiplier applied to censored observations (0.95).
