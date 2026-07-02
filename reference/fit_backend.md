# Identify the backend that produced a fit object

Identify the backend that produced a fit object

## Usage

``` r
fit_backend(raw_fit)
```

## Arguments

- raw_fit:

  a backend-native fit object (an rstan `stanfit` or a cmdstanr
  `CmdStanMCMC`).

## Value

`"rstan"` or `"cmdstanr"`.
