# imuGAP 0.2.0

## Major Features

- **Arbitrary Hierarchy Layer Depth**:
  - `sampling()`, `canonicalize_locations()`, and underlying Stan models now support user-defined hierarchical location structures of arbitrary depth (e.g. 1-layer statewide, 2-layer state-county, 3-layer state-county-school, or deeper regional partitions).
  - Added `assemble_layer_data()` helper to construct and validate hierarchical metadata, bounds, and parent mappings.
  - Added dedicated single-layer model `impute_school_coverage_process_v6_single_layer` with automatic dispatch when single-layer inputs are supplied.
  - Added a new vignette (`user_specified_layers`) demonstrating 1-layer, 2-layer, and 3-layer model estimation and prediction workflows.

- **Backend Abstraction via `flexstanr`**:
  - Integrated `flexstanr (>= 0.2.0)` to provide uniform backend abstraction across `rstan` and `cmdstanr` MCMC engines.
  - Re-exported `flexstanr::stan_options()` for seamless sampler configuration.

- **Modular Stan Architecture**:
  - Refactored top-level Stan models into concise assembly skeletons utilizing modular `#include` directives (`functions/`, `data/`, `transformed_data/`, `parameters/`, `model/`).
  - Pruned legacy/vestigial Stan scripts to streamline code base maintenance.

- **Data Pipeline & Target Generation**:
  - Split target generation into fit-free constructor `create_target()` and validation function `canonicalize_target()`.
  - Added bundled example fitted datasets for single-layer and 2-layer models (`fit_sim_1layer`, `fit_sim_2layer`, `predict_sim_1layer`, `predict_sim_2layer`, `target_sim_1layer`, `target_sim_2layer`).

## Enhancements & Bug Fixes

- **Location Hierarchy Invariant**: Enforced structural validation ensuring location nodes have either 0 offspring (leaf) or $\ge 2$ offspring (preventing single-child degenerate chains).
- **Error Handling & Assertions**: Standardized assertion messages across all functions using module-level `ERR_*` format string constants and helper assertion functions `stop_fmt_if()` and `warn_fmt_if()`.
- **Validation**: Added validation against `NA` weights in population and target specifications.
- **Documentation Examples & Check Times**: Applied combined `@examplesIf interactive()` and `\donttest{}` idiom to computationally heavy examples, enabling fast `pkgdown` builds (~35 seconds) while remaining CRAN `--as-cran` compliant.
- **Vignette Dark Mode Compatibility**: Enforced solid white backgrounds and high-contrast styling across all vignette plots for clean rendering in light/dark modes.

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
  - `impute_school_coverage_process_v6` (adds county- and school-level
    random effects; current default).
- Bundled example datasets for end-to-end examples and tests:
  `locations_sim`, `observations_sim`, `populations_sim`,
  `latent_params_sim`, `predict_sim`, `target_sim`, and the reference
  `fit_sim` `stanfit` fixture.
- pkgdown documentation site published at
  <https://accidda.github.io/imuGAP/>.
