# Dispatch a fit to the chosen backend

The backend is read from `stan_opts$backend` (guaranteed present by the
caller, which requires a
[`stan_options()`](https://accidda.github.io/imuGAP/reference/stan_options.md)
result), so it is not passed separately.

## Usage

``` r
fit_model(model_name, dat_stan, init, stan_opts, drop_pars = NULL)
```

## Arguments

- model_name:

  name of the Stan model; used to look up the compiled model in this
  package's `stanmodels` (rstan) and to locate the `.stan` source file
  under `inst/stan/` (cmdstanr).

- dat_stan:

  the Stan data list.

- init:

  the init list, sized to the chain count.

- stan_opts:

  the validated
  [`stan_options()`](https://accidda.github.io/imuGAP/reference/stan_options.md)
  list (carrying a `backend` element).

- drop_pars:

  character vector of parameter names to exclude from the saved draws,
  or `NULL` to keep everything. Honored by rstan; cmdstanr cannot drop
  parameters and warns if any are requested.

## Value

the backend's fit object (a `stanfit` or `CmdStanMCMC`).
