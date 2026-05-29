## R CMD check results

0 errors | 0 warnings | [N] notes

[Brief note about any notes. Typical notes for a first submission with
Stan models include:

- "New submission" (expected for first CRAN submission).
- Installed package size note: imuGAP bundles compiled Stan models,
  which can push the installed size above the usual thresholds. The
  compiled models are required for the package's core functionality.
- Long examples / installation time: Stan model compilation is
  unavoidable for an rstan-based package; examples that exercise the
  sampler are wrapped in `\dontrun{}` to keep `R CMD check` runtime
  modest.]

## Test environments

- ubuntu-latest (R release, R oldrel, R devel) [GitHub Actions]
- macos-latest (R release, R oldrel, R devel) [GitHub Actions]
- windows-latest (R release, R oldrel, R devel) [GitHub Actions]
- [Add win-builder (devel/release) results once run]
- [Add R-hub results once run]

Local development: [add platform + R version, e.g., macOS 14, R 4.4.x].

## Downstream dependencies

None on CRAN currently. The package is a dependency of the
GitHub-only project `ACCIDDA/imugap-map`.

## Notes for reviewer

First submission. [Placeholder for package-specific notes — examples to
consider mentioning:

- imuGAP wraps Stan models built with `rstantools` and follows the
  standard rstan package layout (`src/Makevars`, `src/stanExports_*`,
  `inst/stan/`).
- Bundled example datasets (`locations_sim`, `observations_sim`,
  `populations_sim`, `fit_sim`) are simulated and small; `fit_sim` is a
  wiring fixture for examples and tests, not a converged posterior.
- Citation guidance: see `inst/CITATION` (or `CITATION.cff` at the
  repo root) for how to cite imuGAP.

<!-- @csmith701: please replace the bracketed placeholders above with package-specific content before release. -->
