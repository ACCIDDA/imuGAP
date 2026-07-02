# Does a fit object carry usable posterior draws?

Detect the degenerate "no draws" case after a fit, so a caller can fail
loudly instead of returning an empty fit. This is backend-aware: rstan
returns a mode-2 `stanfit` with an empty `@sim` when the sampler fails
to initialize (rather than erroring), while cmdstanr exposes its draws
through `$draws()`. Unrecognized objects (e.g. test mocks) are treated
as having draws so they pass through untouched.

## Usage

``` r
backend_has_draws(raw_fit)
```

## Arguments

- raw_fit:

  a backend-native fit object (an rstan `stanfit` or a cmdstanr
  `CmdStanMCMC`).

## Value

logical; `TRUE` if the fit carries usable draws.
