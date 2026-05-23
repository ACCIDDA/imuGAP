# imuGAP 0.1.0

First public release. Initial feature set:

- `sampling()`: fits the imuGAP Bayesian hierarchical coverage model via
  `rstan::sampling()` and returns an `imugap_fit` object wrapping the
  underlying `stanfit` together with model settings and dataset metadata.
- `predict.imugap_fit()`: posterior-predicts coverage probabilities for a
  user-supplied target population grid using `rstan::gqs()`.
- `extract_imugap()`: convenience wrapper around `rstan::extract()` for
  pulling out common imuGAP parameters (defaults to the state-level
  B-spline coefficients `beta_bs`).
- `canonicalize_locations()`, `canonicalize_observations()`,
  `canonicalize_populations()`: validate and convert user-supplied data
  into the canonical forms required by the sampler.
- `imugap_options()`: configures model-side settings (B-spline degrees of
  freedom, dose schedule, model object).
- `stan_options()`: configures Stan sampler settings (`iter`, `chains`,
  `seed`, etc.) with input validation.
- Bundled Stan models:
  - `impute_school_coverage_process_stateonly` (state-level only), and
  - `impute_school_coverage_process_v6` (adds county- and school-level
    random effects; current default).
- Bundled example datasets for end-to-end examples and tests:
  `locations_sim`, `observations_sim`, `populations_sim`, and the
  reference `fit_sim` `stanfit` fixture.
- pkgdown documentation site published at
  <https://accidda.github.io/imuGAP/>.
