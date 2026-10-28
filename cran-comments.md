## Submission Notes for imuGAP 0.3.0

This is a minor release update of imuGAP (version 0.3.0).

### Summary of Changes Since Previous Version (0.2.0)

#### Model Changes

* **Hierarchical Tree Aggregation Solvers**:
  - Introduced calculations to equate parent aggregate coverage to the population-weighted
    sum of child probabilities under nonlinear links (`logit`, `probit`).
  - Added rootfinding approaches `newton`, `halley`, and `builtin`, along with a no-change
    `direct` to translate initial guesses into solved parent aggregation shift offsets.
    `builtin` solves to exhaustion via Stan's general purpose algebraic rootfinder; `newton`
    and `halley` perform tailored limited-step rootfinding; `direct` uses the guess as-is.
  - The solver is selectable at runtime via dynamic dispatch, while the default solver option is
    compiled into an optimized model without branching.

#### User Interface Changes

* **S3 Print Methods**:
  - Added dedicated S3 `print.imugap_fit()` providing a structured summary of the location
    hierarchy, observation data distribution, and key Stan parameters (`beta_bs`,
    `sigma_layer`, `lambda_raw`, and `lp__`).
  - Added dedicated S3 `print.imugap_predict()` displaying target coverage metadata, location
    counts, and posterior prediction draw dimensions.
* **Vignette Architecture & Documentation Overhaul**:
  - Reorganized the vignette suite into focused modular articles: `imuGAP` (core workflow),
    `example_data` (input data schemas and structure), `examining_fits` (posterior
    diagnostics and inspection), and `user_specified_layers` (multi-layer and single-layer
    spatial partitions).
  - Added school-level kindergarten entry records visualization distinguishing individual schools
    by color and illustrating coverage dynamics across cohorts.
  - Standardized active voice, direct visual presentations, and documentation formatting across
    all articles.
* **Structured Input Conditions** (#156):
  - Errors raised through `stop_fmt_if()` have class `imugap_error`; warnings from
    `warn_fmt_if()` have class `imugap_warning`. Both retain base condition classes and carry a
    template `id` and raw values in `fields`. Front-ends can read these fields instead of parsing
    message text. Stan backend errors retain their own classes.
  - Canonicalizer column checks report offending rows in a `rows` field (1-based indices into
    the caller's table) and name the caller's table (`observations`, `populations`).
  - `canonicalize_locations()` rejects `NA` in `loc_id`, and `canonicalize_observations()`
    accepts an all-`NA` logical `censored` column as none censored. An `NA` in a subset-checked
    column is reported as missing, not as "missing NA".

#### Engineering Changes

* **Stan Modular Organization**:
  - Decoupled tree accumulation into modular Stan includes
    (`functions/layer_offsets_static.stan`, `functions/layer_offsets_dispatch.stan`), isolating
    solver dependencies from shared QR basis projection (`functions/layer_offsets.stan`).
  - Standardized top-level model names under the convention
    `<time>_<offsets>_<guess>_<solver>_<link>.stan` (e.g.
    `bspline_static_zero_halley2_logit.stan`, `bspline_static_zero_dispatch_probit.stan`) and
    `<time>_single_<link>.stan` for unpartitioned models.
* **Automated Diagram Pipeline**:
  - Added automated Mermaid diagram compilation pipeline generating SVG and PDF visual assets
    with dark-mode compatibility.
* **Synthetic Benchmark Suite**:
  - Added a benchmarking suite (`data-raw/benchmarks/`) with Slurm HPC array execution to
    profile runtime, effective sample size, numerical accuracy, and calibration across solver
    alternatives and hierarchy depths.
* **Stan Computation Optimization**:
  - Optimized dose transitions, linear predictor broadcasting, and blocked QR orthonormal basis
    calculations.
  - Decoupled observation likelihood evaluation into uncensored, right-censored, and
    left-censored streams for vectorized evaluation.
  - Added population-scaled hierarchical layer offset shrinkage with standardized QR basis
    orientation.
  - Refactored Stan initial value generation into static helper `make_init_fn()`.
* **Build Organization**:
  - Streamlined developer and CI workflows via authoritative `just` recipes (`just lint`,
    `just docs`, `just diagrams`, `just data-fit`).
  - Decoupled `lintr` package execution from `devtools` in CI linting workflows.

## Test environments

Continuous integration (GitHub Actions, `R-CMD-check.yaml`), each run with
`R CMD check --as-cran`:

- ubuntu-latest, R release / oldrel / devel
- macos-latest, R release / oldrel / devel
- windows-latest, R release / oldrel / devel

Local development:

- x86_64-pc-linux-gnu (Linux / Ubuntu), R 4.5.1

## R CMD check results

0 errors | 0 warnings | 1 note

* **Installed package size.** imuGAP is an `rstan`-based package: it bundles
  compiled Stan models (`src/stanExports_*`, `inst/stan/`), which push the
  installed size above the usual threshold. The compiled models are required
  for the package's core functionality and cannot be reduced without removing
  it.

(Sampler-exercising examples are wrapped in `\donttest{}` combined with
`@examplesIf interactive()` to keep check runtime modest and `pkgdown` builds
fast.)

## Downstream dependencies

None on CRAN currently.
