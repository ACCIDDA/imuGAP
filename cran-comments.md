## R CMD check results

0 errors | 0 warnings | 2 notes

This is a new submission.

* **New submission.** First release of imuGAP to CRAN.

* **Installed package size.** imuGAP is an `rstan`-based package: it bundles
  compiled Stan models (`src/stanExports_*`, `inst/stan/`), which push the
  installed size above the usual threshold. The compiled models are required
  for the package's core functionality and cannot be reduced without removing
  it.

* **S3 generic/method consistency** (`summary.imugap_predict`). [csmith701:
  confirm wording. The `summary` method for `imugap_predict` objects takes
  additional arguments beyond the `summary()` generic; tracked separately in
  ACCIDDA/imuGAP#73.]

(Stan model compilation also makes installation and any sampler-exercising
examples slow; such examples are wrapped in `\dontrun{}` to keep check runtime
modest.)

## Test environments

- ubuntu-latest (R release, R oldrel, R devel) [GitHub Actions]: 0 errors, 0 warnings
- macos-latest (R release, R oldrel, R devel) [GitHub Actions]: 0 errors, 0 warnings
- windows-latest (R release, R oldrel, R devel) [GitHub Actions]
- [TODO before submission: win-builder (devel + release) results]
- [TODO before submission: R-hub results]

Local development: [TODO: add platform + R version].

## Downstream dependencies

None on CRAN currently. The package is a dependency of the GitHub-only project
`ACCIDDA/imugap-map`.

## Notes for reviewer

[csmith701 to set the reviewer-facing voice; draft below per @pearsonca on #31.]

First submission. Points that may be useful to the reviewer:

- imuGAP wraps Stan models built with `rstantools` and follows the standard
  rstan package layout (`src/Makevars`, `src/stanExports_*`, `inst/stan/`).
- Bundled example datasets (`locations_sim`, `observations_sim`,
  `populations_sim`, `fit_sim`) are simulated and small; `fit_sim` is a wiring
  fixture for examples and tests, not a converged posterior.
- Citation guidance: see `CITATION.cff` at the repo root for how to cite imuGAP.
