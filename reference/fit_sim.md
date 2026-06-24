# Example Stan Fit

A reference `stanfit` object produced by running
[`imuGAP()`](https://accidda.github.io/imuGAP/reference/imuGAP-package.md)
on the bundled `locations_sim`, `populations_sim`, and
`observations_sim` datasets. Intended as a lightweight fixture for
examples, tests, and downstream tooling that needs a real fit without
paying the cost of recompiling or re-running the Stan model.

## Usage

``` r
fit_sim
```

## Format

A `stanfit` object as returned by
[`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html).

## Details

Generated with the same minimal sampler settings as the smoke test:

- `iter = 100`

- `chains = 1`

- `seed = 1L`

- `refresh = 0`

These settings are not enough for convergence; `fit_sim` is a wiring
fixture, not a scientifically meaningful posterior. It is not tracked in
git: it is regenerated on build by `data-raw/fit_data.R` (run
`just data-fit` locally, or `just data` for the full pipeline).

Note that `stanfit` objects bundle references to the compiled Stan model
and can be sensitive to major version changes in `rstan` and
`StanHeaders`. If a future install fails to load `fit_sim`, regenerate
it via `data-raw/fit_data.R`.
