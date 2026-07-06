# Contributing to imuGAP

Thank you for your interest in contributing to imuGAP! This document
explains how to report issues, propose changes, and what to expect from
the CI pipeline when you open a pull request.

## Code of Conduct

All contributors are expected to be respectful and professional in all
interactions — issues, pull requests, code reviews, and discussions.
Constructive feedback is welcome; personal attacks and dismissive
language are not.

## Reporting Bugs

Use the [bug report template](https://github.com/ACCIDDA/imuGAP/issues/new?template=bug_report.yml)
when opening a new issue. It asks for a description, a minimal reprex,
your `sessionInfo()` output, and your operating system.

## Suggesting Features

Use the [feature request template](https://github.com/ACCIDDA/imuGAP/issues/new?template=feature_request.yml).
Describe the use case and, if possible, how the feature fits into the
existing model workflow (`imuGAP()` -> Stan sampling -> post-processing).
You can also open a blank issue if neither template fits.

## Submitting Changes

1. **Fork & branch.** Create a feature branch off `main` (e.g.
   `fix/obs-population-check` or `feature/predict-mode`).
2. **Make your changes.** Follow the style conventions below.
3. **Test locally.** Run `R CMD check` and the test suite before
   pushing (see the commands below).
4. **Open a pull request** against `main` with a clear description of
   what changed and why.

### Local Development Commands

```sh
# Full rebuild + install (required after editing any .stan file)
R CMD INSTALL --preclean .

# Regenerate documentation from roxygen comments. This writes man/*.Rd,
# NAMESPACE, and R/globals.R -- all roxygen2/roxyglobals output, none tracked in
# git (#53, #115). CI regenerates them on every build, so never hand-edit them.
Rscript -e 'devtools::document()'

# Regenerate the fitted data artifacts (NOT tracked in git, like man/*.Rd).
# fit_sim/target_sim/predict_sim/latent_params_sim must exist before `R CMD
# build`/`check`, the vignette, or the examples will run. Needs a Stan toolchain.
just data-fit   # or: Rscript data-raw/fit_data.R

# Run the test suite
Rscript -e 'devtools::test()'

# Lint the package (config in .lintr)
Rscript -e 'lintr::lint_package()'

# Build and check
R CMD build . && R CMD check imuGAP_*.tar.gz
```

### Style

- R code is linted with `lintr` (configuration in `.lintr`).
  `R/stanmodels.R` and `inst/analysis/` are excluded from linting
  because they are generated or standalone scripts, respectively.
- Do not hand-edit generated files: `R/stanmodels.R`,
  `src/stanExports_*.{cc,h}`, `configure`, and `configure.win` are all
  produced by `rstantools::rstan_config()`; `NAMESPACE`, `R/globals.R`, and
  `man/*.Rd` are produced by `roxygen2::roxygenise()` (via roxygen tags and
  the roxyglobals roclet) and are untracked -- regenerate with
  `devtools::document()`.
- Stan model variants are composed via `#include` toggling in the
  top-level `.stan` files — prefer toggling includes over duplicating
  whole model files.

## The Stan backend is vendored from flexstanr

`R/import-standalone-backends.R` is **not edited here**. It is a standalone file
vendored from [ACCIDDA/flexstanr](https://github.com/ACCIDDA/flexstanr) — the
single source of truth for the portable backend layer shared across imuGAP,
hestia, and SeverityEstimate (see #112). Its header says *"do not edit by
hand."*

- To change backend behavior, edit **flexstanr**, then re-sync here. The
  preferred way is the justfile recipe:
  ```sh
  just update-standalones
  ```
  It re-runs `use_standalone` for each vendored source and restores `DESCRIPTION`
  afterwards. The manual equivalent is:
  ```r
  usethis::use_standalone("ACCIDDA/flexstanr", "backends")
  ```
  but note that call also rewrites `DESCRIPTION` — it strips version pins, e.g.
  `rstan (>= 2.18.1)` → `rstan`
  ([usethis#2198](https://github.com/r-lib/usethis/issues/2198)) — so you must
  **revert that DESCRIPTION change** by hand; the re-sync should only touch
  `R/import-standalone-backends.R`. The recipe does that revert for you.
- The vendored header's `# repo: ACCIDDA/flexstanr` line parses as R, so
  `commented_code_linter` is disabled globally in `.lintr` (rather than excluding
  the file); the vendored file is otherwise linted like any other.
- `use_standalone` is a one-shot copy, so the vendored file can fall behind
  upstream. The **`backends-sync`** workflow (weekly) discovers every vendored
  standalone file, reads each one's source from its `# Source:` header, compares
  against upstream, and — if anything has drifted — opens a pull request with the
  refreshed copies. Review and merge that PR to re-sync.

## Dependencies: cmdstanr, `Remotes`, and the pak gotcha

cmdstanr is an optional, non-CRAN `Suggests`. It is resolved in CI via
**`Remotes: stan-dev/cmdstanr`** in `DESCRIPTION`: pak (used by `r-lib/actions`)
reads `Remotes` but **not** `Additional_repositories`
([r-lib/pak#424](https://github.com/r-lib/pak/issues/424)). The
`Additional_repositories` entry is kept only for base `install.packages` and
`R CMD check --as-cran`.

**Do not** add the stan-dev r-universe as a repository (e.g. an
`extra-repositories` in a workflow): pak would then prefer its **dev** builds of
the whole Stan stack (StanHeaders/rstan), which fail to compile against CRAN's
RcppEigen and break every job. `Remotes` pins *only* cmdstanr and keeps the rest
of the Stan stack on CRAN. (This one has bitten us — see #101.)

## What Happens When You Open a PR

Every pull request triggers two GitHub Actions workflows:

| Workflow | What it does |
|----------|-------------|
| **R-CMD-check** | Runs `R CMD check --as-cran` on Ubuntu, macOS, and Windows across R release, oldrel, and devel (9 jobs total). R-devel jobs are allowed to fail without blocking the PR. Stan compilation artifacts are cached per OS and R version. |
| **pkgdown** | Builds the documentation site. On PRs this is a build-only check (no deployment); the site is deployed to GitHub Pages on pushes to `main`, published releases, and manual workflow dispatches. |

Both workflows should pass before a PR is merged.

## Questions?

If something is unclear, open an issue or comment on an existing one —
we are happy to help.
