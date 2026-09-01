# Contributing to imuGAP

Thank you for your interest in contributing to imuGAP! This document
explains how to develop, test, and propose changes to the package, as
well as the coding conventions, error handling standards, and CI
pipelines enforced across the repository.

------------------------------------------------------------------------

## Code of Conduct

All contributors are expected to be respectful and professional in all
interactions — issues, pull requests, code reviews, and discussions.
Constructive feedback is welcome; personal attacks and dismissive
language are not.

------------------------------------------------------------------------

## Reporting Bugs and Feature Requests

- **Bugs**: Use the [bug report
  template](https://github.com/ACCIDDA/imuGAP/issues/new?template=bug_report.yml).
  Provide a minimal reproducible example (reprex), session info, and
  operating system.
- **Features**: Use the [feature request
  template](https://github.com/ACCIDDA/imuGAP/issues/new?template=feature_request.yml).
  Explain the use case and how it connects to the `imuGAP` workflow
  (`canonicalize` -\> Stan sampling -\> prediction/summary).

------------------------------------------------------------------------

## Development Workflow & `just` Recipes

We use [`just`](https://github.com/casey/just) to automate development
tasks. All recipes handle namespace and environment configuration
automatically:

| Recipe | Description | Equivalent Base Command |
|----|----|----|
| `just` | Run full validation pipeline: clean, format, lint, docs, test | *(compound command)* |
| `just format` | Format R code using `air` | `air format .` |
| `just lint` | Lint R code using `air` and `lintr` | `air format . --check && Rscript -e "lintr::lint_package()"` |
| `just docs` | Regenerate roxygen documentation (`man/`, `R/globals.R`) | `Rscript -e "roxygen2::roxygenize()"` |
| `just install` | Install package into local R library | `R CMD INSTALL .` |
| `just test` | Run complete unit test suite via `testthat` / `devtools` | `Rscript -e "devtools::test()"` |
| `just test-fast` | Run tests, stopping on first failure | `Rscript -e "devtools::test(stop_on_failure = TRUE)"` |
| `just coverage` | Measure test coverage via `covr` | `Rscript -e "covr::package_coverage()"` |
| `just spell` | Check spelling across docs and vignettes via `spelling` | `Rscript -e "spelling::spell_check_package()"` |
| `just render` | Render all vignettes to HTML and PDF | `Rscript -e "rmarkdown::render(...)"` |
| `just site` / `just site-quick` | Fast build of `pkgdown` documentation site (no package reinstall) | `Rscript -e "pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)"` |
| `just site-full` | Full build of `pkgdown` site with package reinstallation (for updated data) | *(compound: install + site)* |
| `just site-preview [port=8000]` | Build and preview pkgdown documentation site on localhost | `Rscript -e "httpuv::runStaticServer(dir = 'docs', port = 8000)"` |
| `just data-inputs` | Regenerate `*_sim` input datasets from raw simulation | `Rscript data-raw/DATASET.R` |
| `just data-fit` | Regenerate pre-computed Stan fits (`fit_sim`, `target_sim`, etc.) | `Rscript data-raw/fit_data.R` |
| `just data` | Regenerate all package data (`data-inputs` + `data-fit`) | *(compound command)* |
| `just build` | Build package `.tar.gz` archive | `R CMD build .` |
| `just check` | Check package archive | `R CMD check imuGAP_*.tar.gz --no-manual --no-tests` |
| `just check-cran` | Check package archive using strict CRAN settings | `R CMD check imuGAP_*.tar.gz --as-cran` |

------------------------------------------------------------------------

## Code Coverage and Spell Checking

### 1. Code Coverage (`covr`)

- Run `just coverage` to measure package test coverage.
- The CI workflow (`.github/workflows/test-coverage.yaml`) runs
  [`covr::codecov()`](http://covr.r-lib.org/reference/codecov.md) on
  every pull request and uploads reports to Codecov.
- Aim to maintain high coverage (\>90%, targeting 100%) across all
  active R source files (`R/canonicalize.R`, `R/checkers.R`,
  `R/helpers.R`, `R/imuGAP.R`, `R/methods.R`, `R/options.R`).
- **Covered vs. Ignored Files (`.covrignore`)**:
  - `src/*.{cc,cpp,h}`: Generated C++ Stan headers and model exports
    compiled by `rstantools` from `inst/stan/*.stan`. They cannot be
    instrumented directly by `covr`; the underlying models are verified
    through integration tests
    ([`sampling()`](https://accidda.github.io/imuGAP/reference/sampling.md),
    [`predict()`](https://rdrr.io/r/stats/predict.html)).
  - `R/stanmodels.R`: Generated Stan model loader emitted by
    [`rstantools::rstan_config()`](https://mc-stan.org/rstantools/reference/rstan_config.html).
  - `R/flexstanr.R`: Generated backend integration shim emitted by
    [`flexstanr::use_flexstanr()`](https://accidda.github.io/flexstanr/reference/use_flexstanr.html).

### 2. Spell Checking (`spelling`)

- Run `just spell` to check spelling across all `.Rd` documentation,
  vignettes, and `README.md`.
- Legitimate technical terms, package names, author names, or domain
  vocabulary are maintained in `inst/WORDLIST`. Update the list with
  `Rscript -e "spelling::update_wordlist()"`.

------------------------------------------------------------------------

## Code Style, Linting, and Documentation

### 1. Formatting & Linting

- R code is formatted with `air` and linted with `lintr` (rules in
  `.lintr`).
- Maximum line length is **100 characters**.
- `R/stanmodels.R`, `R/flexstanr.R`, `inst/analysis/`, `inst/scripts/`,
  and `data-raw/` are excluded from linting because they are generated
  artifacts or standalone scratch scripts.

### 2. Untracked Artifacts & Generated Files

- **Do not hand-edit generated files**:
  - `R/globals.R` and `man/*.Rd` are produced by
    [`roxygen2::roxygenise()`](https://roxygen2.r-lib.org/reference/roxygenize.html)
    (via `roxygen2` and `roxyglobals`) and are untracked (#53).
    Regenerate them with `just docs`.
  - Pre-computed fitted data artifacts (`data/fit_sim*.rda`,
    `data/predict_sim*.rda`, `data/target_sim*.rda`) are untracked and
    generated via `just data-fit`.
  - `R/flexstanr.R` is generated by
    [`flexstanr::use_flexstanr()`](https://accidda.github.io/flexstanr/reference/use_flexstanr.html).
- **Exported Datasets**: Document datasets with the `@name <data>` /
  `@docType data` idiom in `R/imuGAP-package.R`.

### 3. Markdown Documentation

- `roxygen2` markdown mode is enabled
  (`Roxygen: list(markdown = TRUE)`).
- Prefer standard markdown syntax:
  - Use backticks for code identifiers, arguments, and return types
    (e.g. `` `locations` ``, `` `data.table` ``).
  - Use cross-reference markdown links (e.g. `[sampling()]`,
    `[flexstanr::stan_options()]`).
  - Use markdown lists, bold text, and tables rather than raw `\code{}`,
    `\link{}`, or `\tabular{}` Rd tags.

### 4. Roxygen Examples: Dual `@examplesIf` and `\donttest` Pattern

For computationally heavy functions (such as
[`sampling()`](https://accidda.github.io/imuGAP/reference/sampling.md)
or multi-draw [`predict()`](https://rdrr.io/r/stats/predict.html)):

- **Always combine `@examplesIf interactive()` with `\donttest{}`**:

  ``` r

  #' @examplesIf interactive()
  #' \donttest{
  #' data("locations_sim")
  #' data("observations_sim")
  #' data("populations_sim")
  #' st_opts <- stan_options(chains = 2, iter = 500)
  #' sampling(
  #'   observations_sim, populations_sim, locations_sim,
  #'   stan_opts = st_opts
  #' )
  #' }
  ```

- **Why both are necessary**:

  - `pkgdown` runs `\donttest{}` blocks during site builds;
    `@examplesIf interactive()` evaluates to `FALSE` during
    non-interactive batch builds, keeping site build time fast (~35
    seconds instead of \>25 minutes).
  - CRAN checks (`R CMD check --as-cran`) look for `\donttest{}` to skip
    lengthy runtime checks during package validation.
  - Interactive user sessions (`example(sampling)`) execute normally.

### 5. Modular Stan Architecture

- Stan models in `imuGAP` are designed modularly.
- Top-level Stan models directly in `inst/stan/` (and not Stan code in
  subdirectories) must remain concise assembly skeletons composed of
  `#include <subpath>.stan` directives for particular modular elements
  (`functions/`, `data/`, `transformed_data/`, `parameters/`, `model/`,
  `generated_quantities/`).
- Never inline full block contents or raw logic directly into top-level
  models in `inst/stan/`; keep component logic encapsulated in dedicated
  sub-files to facilitate reuse, maintainability, and clean diffs.

### 6. Vignette Plot Styling & Dark Mode Compatibility

To ensure plots remain clear and readable regardless of whether users
view the pkgdown site in light or dark mode:

- In vignette setup chunks, specify
  `knitr::opts_chunk$set(dev.args = list(bg = "white"))`.
- Disable automatic plot theme inversion with
  `if (requireNamespace("thematic", quietly = TRUE)) thematic::thematic_off()`.
- Configure
  [`ggplot2::theme_set()`](https://ggplot2.tidyverse.org/reference/get_theme.html)
  with solid white backgrounds (`plot.background`, `panel.background`,
  `legend.background`) and black text (`text`, `axis.text`,
  `axis.title`, `plot.title`).

### 7. Package Reinstallation & Vignette Data

Vignette chunks load data using `data(..., package = "imuGAP")`, which
resolves datasets from the **installed package library** rather than the
working directory. When troubleshooting vignette (and related `pkgdown`
site) issues associated with rendering package example data, if the fix
ends up being in the package data (`data-raw/DATASET.R` or
`data-raw/fit_data.R`), you must reinstall the package (`just install`
or `R CMD INSTALL .`) before re-rendering vignettes or rebuilding the
site with updated data (or use `just site-full`).

------------------------------------------------------------------------

## Error Messages and Signaling Standards

All user-facing validation errors and warnings should follow these
standards:

### 1. Centralized Format String Constants

- Define error and warning message format strings as constants at the
  top of each R file prefixed with `ERR_` or `MSG_`:

  ``` r

  ERR_MUST_BE_INTEGER <- "`%s` column '%s' must contain integers"
  ERR_CANNOT_HAVE_NA <- "`%s` column '%s' cannot contain NA values"
  ERR_OPT_UNKNOWN_MODEL <- "`imugap_opts` unknown model '%s'"
  ```

### 2. Signaling Functions: `stop_fmt_if` and `warn_fmt_if`

- Use internal helpers
  [`stop_fmt_if()`](https://accidda.github.io/imuGAP/reference/stop_fmt_if.md)
  and
  [`warn_fmt_if()`](https://accidda.github.io/imuGAP/reference/warn_fmt_if.md)
  for validation assertions:

  ``` r

  stop_fmt_if(
    !all(as.integer(dt[, get(x)]) == dt[, get(x)]),
    ERR_MUST_BE_INTEGER,
    deparse(substitute(dt)),
    x,
    n = n + 1L
  )
  ```

- Use the parameter `n` to adjust the call stack offset so the error is
  attributed to the user’s top-level function call rather than internal
  helper functions.

### 3. Typography: Backticks vs. Single Quotes

Follow a strict convention when formatting error and warning strings:

- **Backticks (`` `code` ``)**: Use for formal R code symbols, argument
  names, function names, expressions, and classes:
  - `` `observations` must be a data.frame ``
  - `` `df` must be a single positive integer ``
  - `` `stan_opts` must be created by stan_options() ``
- **Single Quotes (`'value'`)**: Use for user-supplied string values,
  column names, model names, or discrete inputs:
  - `column '%s' cannot contain NA values`
  - `unknown model '%s'`
  - `'%s' must be numeric`

------------------------------------------------------------------------

## Stan Backend and Dependencies

- **`flexstanr`**: Portable Stan backend support is provided by the
  imported package `flexstanr (>= 0.2.0)`. The integration helper
  `R/flexstanr.R` is generated by
  [`flexstanr::use_flexstanr()`](https://accidda.github.io/flexstanr/reference/use_flexstanr.html).
- **`cmdstanr`**: An optional, non-CRAN `Suggests`. It is resolved in CI
  via **`Remotes: stan-dev/cmdstanr`** in `DESCRIPTION`.
- **Stan Stack Pinning**: **Do not** add the `stan-dev` r-universe as an
  extra repository in CI workflows: `pak` would then select dev builds
  of `StanHeaders`/`rstan`, which fail to compile against CRAN’s
  `RcppEigen` (#101). `Remotes` pins *only* `cmdstanr` while keeping the
  remainder of the Stan stack on CRAN.

------------------------------------------------------------------------

## Pull Request and CI Workflows

Every pull request triggers four automated GitHub Actions workflows:

1.  **`R-CMD-check`**: Runs `R CMD check --as-cran` across Ubuntu,
    macOS, and Windows on R release, oldrel, and devel (9 jobs).
2.  **`lint`**: Verifies formatting with `air format . --check` and lint
    rules with
    [`lintr::lint_package()`](https://lintr.r-lib.org/reference/lint.html).
3.  **`test-coverage`**: Computes code coverage with `covr` and uploads
    results to Codecov.
4.  **`pkgdown`**: Builds the documentation site and confirms that all
    vignettes compile cleanly. Deployed to GitHub Pages upon push to
    `main` and published releases.

All checks must pass before merging.
