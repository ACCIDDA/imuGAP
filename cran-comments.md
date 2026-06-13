## Test environments

Continuous integration (GitHub Actions, `R-CMD-check.yaml`), each run with
`R CMD check --as-cran`:

- ubuntu-latest, R release / oldrel / devel
- macos-latest, R release / oldrel / devel
- windows-latest, R release / oldrel / devel

Local development:

- aarch64-apple-darwin (macOS), R 4.5.3

win-builder (devel + release) and R-hub: pending pre-submission check. These
are run with `devtools::check_win_devel()`, `devtools::check_win_release()`,
and `rhub::rhub_check()` immediately before submission, and their results are
recorded here at that point.

## R CMD check results

0 errors | 0 warnings | 1 note

This is a new submission.

* **New submission.** First release of imuGAP to CRAN.

* **Installed package size.** imuGAP is an `rstan`-based package: it bundles
  compiled Stan models (`src/stanExports_*`, `inst/stan/`), which push the
  installed size above the usual threshold. The compiled models are required
  for the package's core functionality and cannot be reduced without removing
  it.

(Stan model compilation also makes installation and any sampler-exercising
examples slow; such examples are wrapped in `\dontrun{}` to keep check runtime
modest.)

## Downstream dependencies

None on CRAN currently. The package is a dependency of the GitHub-only project
`ACCIDDA/imugap-map`.

## Notes for reviewer

First submission. Points that may be useful to the reviewer:

- imuGAP wraps Stan models built with `rstantools` and follows the standard
  rstan package layout (`src/Makevars`, `src/stanExports_*`, `inst/stan/`).
- Bundled example datasets (`fit_sim`, `latent_params_sim`, `locations_sim`,
  `observations_sim`, `populations_sim`, `predict_sim`, `target_sim`) are
  simulated and small.
- Citation guidance: see `CITATION.cff` at the repo root for how to cite imuGAP.
