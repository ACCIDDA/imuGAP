## Submission Notes for imuGAP 0.3.0

This is a minor release update of imuGAP (version 0.3.0).

### Summary of Changes Since Previous Version (0.2.0)

* **S3 Print Methods**:
  - Added dedicated S3 `print.imugap_fit()` providing a structured overview of the location
    hierarchy, observation statistics, and summary of primary non-offset Stan parameters
    (`beta_bs`, `sigma_layer`, `lambda_raw`, and `lp__`).
  - Added dedicated S3 `print.imugap_predict()` displaying target population metadata, location
    counts, and posterior prediction draw dimensions.
* **Vignette Architecture & Documentation Suite**:
  - Reorganized vignette documentation into a clean, modular multi-article suite: `imuGAP` (core
    workflow overview), `example_data` (input data schemas, structure, and visualization),
    `examining_fits` (posterior model inspection and diagnostics), and `user_specified_layers`
    (spatial hierarchy configurations).
  - Added school-level kindergarten entry records visualization distinguishing individual schools
    by color and illustrating coverage dynamics across cohorts.
  - Standardized active voice, direct visual presentations, and documentation formatting across
    all articles.
* **Diagram Generation Pipeline**:
  - Added automated Mermaid diagram compilation pipeline generating SVG and PDF visual assets
    with dark-mode filter compatibility.
* **Stan Performance & Model Optimizations**:
  - Optimized dose transition convolutions, linear predictor broadcasting, and blocked QR
    orthonormal basis calculations.
  - Decoupled observation likelihood evaluation into uncensored, right-censored, and left-censored
    streams for vectorized evaluation.
  - Added population-scaled hierarchical layer offset shrinkage with standardized QR basis
    orientation.
  - Refactored Stan initial value generation into static helper `make_init_fn()`.
* **Infrastructure & CI**:
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

- x86_64-pc-linux-gnu (Linux / Ubuntu), R 4.4.2

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
