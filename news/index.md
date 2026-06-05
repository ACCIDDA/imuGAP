# Changelog

## imuGAP 0.1.0

First public release. Initial feature set:

- [`sampling()`](https://accidda.github.io/imuGAP/reference/sampling.md):
  fits the imuGAP Bayesian hierarchical coverage model via
  [`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
  and returns an `imugap_fit` object wrapping the underlying `stanfit`
  together with model settings and dataset metadata.
- [`predict.imugap_fit()`](https://accidda.github.io/imuGAP/reference/predict.imugap_fit.md):
  posterior-predicts coverage probabilities for a user-supplied target
  population grid using
  [`rstan::gqs()`](https://mc-stan.org/rstan/reference/stanmodel-method-gqs.html).
- [`extract_imugap()`](https://accidda.github.io/imuGAP/reference/extract_imugap.md):
  convenience wrapper around
  [`rstan::extract()`](https://mc-stan.org/rstan/reference/stanfit-method-extract.html)
  for pulling out common imuGAP parameters (defaults to the state-level
  B-spline coefficients `beta_bs`).
- [`canonicalize_locations()`](https://accidda.github.io/imuGAP/reference/canonicalize.md),
  [`canonicalize_observations()`](https://accidda.github.io/imuGAP/reference/canonicalize.md),
  [`canonicalize_populations()`](https://accidda.github.io/imuGAP/reference/canonicalize.md):
  validate and convert user-supplied data into the canonical forms
  required by the sampler.
- [`imugap_options()`](https://accidda.github.io/imuGAP/reference/imugap_options.md):
  configures model-side settings (B-spline degrees of freedom, dose
  schedule, model object).
- [`stan_options()`](https://accidda.github.io/imuGAP/reference/stan_options.md):
  configures Stan sampler settings (`iter`, `chains`, `seed`, etc.) with
  input validation.
- Bundled Stan models:
  - `impute_school_coverage_process_v6` (adds county- and school-level
    random effects; current default).
- Bundled example datasets for end-to-end examples and tests:
  `locations_sim`, `observations_sim`, `populations_sim`,
  `latent_params_sim`, `predict_sim`, `target_sim`, and the reference
  `fit_sim` `stanfit` fixture.
- pkgdown documentation site published at
  <https://accidda.github.io/imuGAP/>.
