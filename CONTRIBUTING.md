# Contributing to imuGAP

Thank you for your interest in contributing to imuGAP! This document explains how to develop, test, and propose changes to the package, as well as the coding conventions, error handling standards, and CI pipelines enforced across the repository.

---

## Code of Conduct

All contributors are expected to be respectful and professional in all interactions — issues, pull requests, code reviews, and discussions. Constructive feedback is welcome; personal attacks and dismissive language are not.

---

## Reporting Bugs and Feature Requests

* **Bugs**: Use the [bug report template](https://github.com/ACCIDDA/imuGAP/issues/new?template=bug_report.yml). Provide a minimal reproducible example (reprex), session info, and operating system.
* **Features**: Use the [feature request template](https://github.com/ACCIDDA/imuGAP/issues/new?template=feature_request.yml). Explain the use case and how it connects to the `imuGAP` workflow (`canonicalize` -> Stan sampling -> prediction/summary).

---

## Development Workflow & `just` Recipes

We use [`just`](https://github.com/casey/just) to automate development tasks. The `justfile` serves as the **single authoritative reference** for repository build, documentation, verification, and diagram actions across both local development and GitHub Actions CI:

| Recipe | Description | Equivalent Base Command |
|---|---|---|
| `just` | Run standard validation pipeline: format, lint, docs, test | *(compound command)* |
| `just clean` | Clean up build, check, and rendered diagram artifacts | *(compound command)* |
| `just format` | Format R code using `air` | `air format .` |
| `just lint` | Lint R code and verify Rcpp export bindings | `air format . --check && Rscript -e "lintr::lint_package()" && just check-rcpp` |
| `just check-rcpp` | Check that Rcpp export bindings in `src/` are synchronized with Stan models | `Rscript -e "Rcpp::compileAttributes()" && git diff --exit-code src/RcppExports.cpp` |
| `just docs` | Regenerate roxygen documentation (`man/`, `R/globals.R`) | `Rscript -e "roxygen2::roxygenize()"` |
| `just install` | Install package into local R library | `R CMD INSTALL .` |
| `just test` | Run complete unit test suite via `testthat` / `devtools` | `Rscript -e "devtools::test()"` |
| `just test-fast` | Run tests, stopping on first failure | `Rscript -e "devtools::test(stop_on_failure = TRUE)"` |
| `just coverage` | Measure test coverage via `covr` | `Rscript -e "covr::package_coverage()"` |
| `just spell` | Check spelling across docs and vignettes via `spelling` | `Rscript -e "spelling::spell_check_package()"` |
| `just render` | Render all vignettes to HTML and PDF | `Rscript -e "rmarkdown::render(...)"` |
| `just diagrams` | Compile Mermaid source diagrams (`.mmd` to `.svg`/`.pdf`) | `mmdc -i ... -o ...` |
| `just site` / `just site-quick` | Fast build of `pkgdown` documentation site (no package reinstall) | `Rscript -e "pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)"` |
| `just site-full` | Full build of `pkgdown` site with package reinstallation (for updated data) | *(compound: install + site)* |
| `just site-preview [item=""] [port=8000]` | Preview pkgdown site on localhost (supports targeted item, e.g. `just site-preview imuGAP`) | `httpuv::runStaticServer(...)` |
| `just data-inputs` | Regenerate `*_sim` input datasets from raw simulation | `Rscript data-raw/DATASET.R` |
| `just data-fit-main` | Regenerate core 3-layer Stan fits (`fit_sim`, `predict_sim`) | `Rscript data-raw/fit_data.R` |
| `just data-fit-layers` | Regenerate 1-layer and 2-layer ablation models | `Rscript data-raw/fit_layers.R` |
| `just data-fit-school-fold [index=1]` | Run a single leave-school-out fold | `Rscript data-raw/fit_single_school_out.R --index 1` |
| `just data-fit-school-cv` | Run all 10 leave-school-out folds and consolidate | *(compound script)* |
| `just data-fit` | Regenerate all pre-computed Stan fits across all models | `just data-fit-main data-fit-layers data-fit-school-cv` |
| `just data` | Regenerate all package data (`data-inputs` + `data-fit`) | *(compound command)* |
| `just build` | Build package `.tar.gz` archive | `R CMD build .` |
| `just check` | Check package archive | `R CMD check imuGAP_*.tar.gz --no-manual --no-tests` |
| `just check-cran` | Check package archive using strict CRAN settings | `R CMD check imuGAP_*.tar.gz --as-cran` |

---

## Code Coverage and Spell Checking

### 1. Code Coverage (`covr`)

* Run `just coverage` to measure package test coverage.
* The CI workflow (`.github/workflows/test-coverage.yaml`) runs `covr::codecov()` on every pull request and uploads reports to Codecov.
* Aim to maintain high coverage (>90%, targeting 100%) across all active R source files (`R/canonicalize.R`, `R/checkers.R`, `R/helpers.R`, `R/imuGAP.R`, `R/methods.R`, `R/options.R`).
* **Covered vs. Ignored Files (`.covrignore`)**:
  * `src/*.{cc,cpp,h}`: Generated C++ Stan headers and model exports compiled by `rstantools` from `inst/stan/*.stan`. They cannot be instrumented directly by `covr`; the underlying models are verified through integration tests (`sampling()`, `predict()`).
  * `R/stanmodels.R`: Generated Stan model loader emitted by `rstantools::rstan_config()`.
  * `R/flexstanr.R`: Generated backend integration shim emitted by `flexstanr::use_flexstanr()`.

### 2. Spell Checking (`spelling`)

* Run `just spell` to check spelling across all `.Rd` documentation, vignettes, and `README.md`.
* Legitimate technical terms, package names, author names, or domain vocabulary are maintained in `inst/WORDLIST`. Update the list with `Rscript -e "spelling::update_wordlist()"`.

---

## Code Style, Linting, and Documentation

### 1. Formatting & Linting

* R code is formatted with `air` and linted with `lintr` (rules in `.lintr`).
* Maximum line length is **100 characters** for R code, comments, and scripts. In documentation vignettes (`vignettes/*.Rmd`), do not insert hard line breaks when writing or managing text paragraphs and list items (keep each paragraph or list item description on a single unbroken line).
* `R/stanmodels.R`, `R/flexstanr.R`, `inst/analysis/`, `inst/scripts/`, and `data-raw/` are excluded from linting because they are generated artifacts or standalone scratch scripts.

### 2. Tracked vs. Untracked Artifacts & Generated Files

* **Untracked Generated Files** (do not commit; rebuilt automatically during build/CI):
  * `R/globals.R` and `man/*.Rd` are produced by `roxygen2::roxygenise()` (via `roxygen2` and `roxyglobals`) and are untracked (#53). Regenerate them with `just docs`.
  * Pre-computed fitted data artifacts (`data/fit_sim*.rda`, `data/predict_sim*.rda`, `data/target_sim*.rda`, `data/leave_school_out*.rda`) are untracked and generated via `just data-fit`.
  * Diagram assets (`vignettes/figures/*.svg`, `vignettes/figures/*.pdf`, `vignettes/figures/*.png`) are generated from Mermaid `.mmd` files via `just diagrams`.
  * `R/flexstanr.R` is generated by `flexstanr::use_flexstanr()`.
* **Tracked Generated Files**:
  * `src/RcppExports.cpp` is generated by `Rcpp::compileAttributes()` and **must remain tracked in git**. When users install directly from GitHub (e.g. via `devtools::install_github()`, `remotes::install_github()`, or `pak::pak()`), R's installer downloads only git-tracked files and invokes `R CMD INSTALL` without running developer-side attribute compilation. Tracking `src/RcppExports.cpp` ensures DLL symbols and module registration routines (`R_init_imuGAP()`) are present for compilation.
  * Synchronization of `src/RcppExports.cpp` with Stan models and Rcpp attributes is enforced automatically by `just check-rcpp` (run during `just lint` and CI) and by the unit test in `tests/testthat/test-rcpp_exports.R`.
* **Exported Datasets**: Document datasets with the `@name <data>` / `@docType data` idiom in `R/imuGAP-package.R`.

### 3. Roxygen Documentation Conventions

* **Explicit `@title` and `@description`**: Always provide explicit `@title` and `@description` tags in roxygen blocks rather than relying on roxygen2's automatic inference from the initial paragraphs.
* **`data.table` and `@autoglobal`**: Functions performing calculations or non-standard evaluation with `data.table` should generally be marked with `@autoglobal` so that `roxyglobals` automatically registers referenced columns and symbols in `R/globals.R`.
* **Internal Functions**: Unexported helper functions should be tagged with `@keywords internal` and `@noRd` so they are fully documented in source code without generating unneeded `.Rd` manual files.
* **Casing & Punctuation for `@param` and `@return`**:
  * All `@param` descriptions should lead with a lowercase letter (e.g. `a [data.frame()]`, `integer vector`, `logical scalar; ...`).
  * Descriptions should end with a terminating period (`.`).
* **Parameter (`@param`) Formatting**:
  * **Types**: Explicitly state input types/classes using the minimal type that will work (e.g. `a [data.frame()]` rather than compound `[data.frame()] or [data.table()]`, `integer vector`, `an object of class \`imugap_fit\``).
  * **Flag Parameters**: Frame descriptions for boolean/logical flags as questions (e.g. `logical scalar; drop extraneous columns? (default: \`TRUE\`).`, `logical; allow \`NA\` values? (default: \`FALSE\`).`).
  * **Defaults**: Standardize default value notation using `(default: <val>)`, e.g. `(default: 5L)`, `(default: "snapshot")`, `(default: NULL)`.
  * **Ellipsis (`...`)**: Document `...` explicitly as forwarded (`additional arguments passed to [target_fn()].`) or ignored (`additional arguments (currently ignored).`), or document forwarded dots via `@inheritDotParams <pkg>::<fn>`.
* **Return Value (`@return`) Formatting**:
  * Always document the return type and structure leading with `a <type>, ...explanation...` in lowercase (e.g. `a [data.table()], containing...`, `an object of class \`imugap_predict\`, wrapping...`, `a logical scalar, indicating whether...`).
  * For side-effect or validation functions, state invisible returns explicitly (e.g. `invisibly returns \`TRUE\` on success.`).
  * For multi-element lists, use an indented markdown bullet list detailing element names in backticks and types.
* **Markdown Formatting**: `roxygen2` markdown mode is enabled (`Roxygen: list(markdown = TRUE)`). Prefer standard markdown syntax:
  * Use backticks for code identifiers, arguments, and return types (e.g. `` `locations` ``, `` `data.table` ``).
  * Use cross-reference markdown links (e.g. `[sampling()]`, `[flexstanr::stan_options()]`).
  * Use markdown lists, bold text, and tables rather than raw `\code{}`, `\link{}`, or `\tabular{}` Rd tags.

### 4. Roxygen Examples: Dual `@examplesIf` and `\donttest` Pattern

For computationally heavy functions (such as `sampling()` or multi-draw `predict()`):

* **Always combine `@examplesIf interactive()` with `\donttest{}`**:

  ```r
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

* **Why both are necessary**:
  * `pkgdown` runs `\donttest{}` blocks during site builds; `@examplesIf interactive()` evaluates to `FALSE` during non-interactive batch builds, keeping site build time fast (~35 seconds instead of >25 minutes).
  * CRAN checks (`R CMD check --as-cran`) look for `\donttest{}` to skip lengthy runtime checks during package validation.
  * Interactive user sessions (`example(sampling)`) execute normally.

### 5. Modular Stan Architecture

* Stan models in `imuGAP` are designed modularly.
* Top-level Stan models directly in `inst/stan/` (and not Stan code in subdirectories) must remain concise assembly skeletons composed of `#include <subpath>.stan` directives for particular modular elements (`functions/`, `data/`, `transformed_data/`, `parameters/`, `model/`, `generated_quantities/`).
* Never inline full block contents or raw logic directly into top-level models in `inst/stan/`; keep component logic encapsulated in dedicated sub-files to facilitate reuse, maintainability, and clean diffs.

### 6. Vignette Voice, Plot Styling & Dark Mode Compatibility

To ensure vignettes provide clear, engaging, and robust guidance for users:

* **Active Voice & Tone**: Write vignettes directly to the user in the active voice for actions (e.g. "You can fit the model to your data by calling `sampling()`...", "Configure your sampler with `stan_options()`..."). Avoid passive or impersonal constructions (e.g. avoid "Model fitting is executed via...", "Calculations are performed by..."). When introducing visual presentations, diagrams, or rendered plots (describing something to see rather than an action the reader performs), introduce the visual directly (e.g. "The following diagram shows...", "The following plot compares...").
* **Solid Backgrounds**: In vignette setup chunks, specify `knitr::opts_chunk$set(dev.args = list(bg = "white"))`.
* **Thematic Inversion**: Disable automatic plot theme inversion with `if (requireNamespace("thematic", quietly = TRUE)) thematic::thematic_off()`.
* **Plot Design & In-Plot Labels**:
  * **Minimize Non-Data Elements**: Avoid redundant plot titles or subtitles when chunk figure captions (`fig.cap`) already describe the visual. Minimize chartjunk and avoid cluttered legends when direct in-plot labels can clearly identify series or regions.
  * **Unexpanded Axes**: Prefer not expanding axes (`expand = c(0, 0)` in `scale_*_continuous()` or `coord_cartesian(expand = FALSE)`) to keep data bounds tight and crisp.
  * **In-Plot Area & Category Labels**: Prefer direct in-plot annotations (e.g. distinguishing quantitative threshold regions or observation categories directly on the plot canvas) over external legend keys.
* **Coordinate System vs. Scale Limits**: Prefer ggplot2 coordinate system bounds (`coord_cartesian(xlim = ..., ylim = ...)`) over scale-based limits (`scale_*_continuous(limits = ...)`) when zooming or adjusting visible ranges. Scale limits discard data points outside the window (altering summary statistics, regressions, or ribbon clipping), whereas coordinate zooming retains all underlying data.
* **User-Facing Code Conventions**: While `data.table` conventions are used throughout package internals, code displayed to users in vignettes should balance readability and idiomatic usage:
  * **Filtering & Slicing**: Prefer base-R `subset(some_dt, ...)` over `some_dt[...]`.
  * **Single Column Extraction**: Use `$` accessors (e.g. `some_dt$col`) only when extracting a single column vector for use, avoiding compound vector comparisons like `thing$col == ... & thing$col2 == ...`.
  * **In-Place Column Mutations**: When adding or modifying columns on a `data.table`, prefer `data.table` in-place assignment (`dt[, col := ...]`) over dollar-assignment (`dt$col <- ...`).
  * **Derived Subsets & Transforms**: Prefer `within(subset(...), col <- val)` over `transform(...)`.
  * **Row Index Lookups**: Use `data.table`'s `dt[condition, which = TRUE]` rather than `which(thing$col == ...)`.

### 7. Vignette Mermaid Diagrams & Full PDF Support

Vignettes can include process workflows and structural diagrams defined using Mermaid:

* **Source vs. Untracked Artifacts**: The fundamental tracked source artifact is the Mermaid specification in `vignettes/figures/*.mmd`. Generated image files (`vignettes/figures/*.svg`, `vignettes/figures/*.pdf`, `vignettes/figures/*.png`) are untracked (gitignored).
* **Automatic Compilation**: The recipe `just diagrams` (invoking `mmdc` / `@mermaid-js/mermaid-cli`) generates both SVG (for HTML/pkgdown) and PDF (for LaTeX pdflatex builds) prior to building docs, running R CMD check, or rendering vignettes.
* **Embedding**: Include diagrams in vignette `.Rmd` files via `knitr::include_graphics("figures/<name>.svg")`. Always list the SVG file under `resource_files:` in the YAML frontmatter so `pkgdown` copies and discovers the asset.
* **Dark Mode Styling**: Diagrams are styled independently from statistical plots. In `pkgdown/extra.css`, `html[data-bs-theme="dark"] .figure img[src$=".svg"]` applies a dark-mode filter inversion (`invert(0.88) hue-rotate(180deg)`) allowing the diagram to seamlessly blend with dark themes while maintaining crisp line and text contrast.
* **Flow Diagram Design Guidelines**:
  * Exclude step numbers from container box labels; sequential order is conveyed by diagram wiring.
  * Align primary container labels toward the top of each box.
  * Keep secondary detail text styling close in weight/color to the container title so that contrast remains clear under color scheme inversions.

### 8. Package Reinstallation & Vignette Data

Vignette chunks load data using `data(..., package = "imuGAP")`, which resolves datasets from the **installed package library** rather than the working directory. When troubleshooting vignette (and related `pkgdown` site) issues associated with rendering package example data, if the fix ends up being in the package data (`data-raw/DATASET.R` or `data-raw/fit_data.R`), you must reinstall the package (`just install` or `R CMD INSTALL .`) before re-rendering vignettes or rebuilding the site with updated data (or use `just site-full`).

---

## Unit Testing Stan Include Components

Stan code in `imuGAP` is organized into modular include files in `inst/stan/` (across `functions/`, `transformed_data/`, `model/`, etc.). To ensure individual Stan elements function as intended in isolation, we maintain a dedicated Stan unit testing suite in `tests/testthat/`.

### 1. Authoring Unit Tests for New Stan Include Files

When adding or refactoring Stan include files, create unit tests following these guidelines:

* **Explicit Target Declaration & Pipelined Harness**: Declare `target <- "<subpath>.stan"` at the top of the test file, pass `target` to `skip_if_stan_unchanged(target)`, and assemble the model harness via `sprintf(...) |> compile_stan_harness()`.
* **Dynamic Expectations from Input Relationships**: Express test data dimensions and assertions dynamically using input variables and mathematical relationships (e.g. `length(x)`, `nrow(mat)`, `c(tail(lbounds, -1) - 1L, ubound)`, analytical closed forms) rather than hardcoding magic numbers repeatedly.
* **Test via `rstan` Deterministically**: Use the internal test helper `run_stan_harness()` (defined in `tests/testthat/helper-stan-test.R`), which executes `rstan::sampling()` using `algorithm = "Fixed_param"`, `iter = 1`, `warmup = 0`, `chains = 1`, and a fixed random seed.
* **Direct Parameter Extraction & Auto-Reshaping**: `run_stan_harness()` optionally receives a parameter symbol/name (e.g. `run_stan_harness(model, data = ..., out_bounds)`) which extracts and reshapes the single-iteration draw to strip the leading singleton iteration dimension (returning a scalar, vector, matrix, or array directly).
* **Deterministic Verification**:
  * **Functions & Transformed Data**: Pass fixed deterministic test data in `data` and assign computed values to `generated quantities` variables for direct extraction and assertion with `expect_equal()`.
  * **Likelihood & Model Priors**: For files evaluating `target += ...` or `~`, use `run_stan_harness(..., return_fit = TRUE)` and evaluate `rstan::log_prob(fit, upars = c(0.0), adjust_transform = FALSE)`. Note that Stan's sampling statement `~` drops normalization constants with respect to parameters, so test against unnormalized log-densities (e.g. `sum(dbinom(...) - lchoose(...))`).
  * **1D Array Wrapping**: Wrap 1D integer/numeric arrays in `data` with `as.array()` (e.g. `obs_to_weights_bounds = as.array(1L)`) so Rstan does not collapse them into scalars.
* **Smart Change-Detection Caching**:
  * Guard every Stan test block with `skip_if_not_installed("rstan")` and `skip_if_stan_unchanged(target)`.
  * In **local development**, `skip_if_stan_unchanged()` caches MD5 hashes in `tempdir()` to skip model recompilation (~20–25s per model) when the tested Stan files have not changed.
  * During **full checks** (`R CMD check`, `_R_CHECK_PACKAGE_NAME_`, or `CI`), caching is completely bypassed — tests run unconditionally and do not read or write the local cache.

### 2. Stan Include Coverage Mapping

| Stan Subdirectory | Stan File / Module | Test File | Test Focus & Verification |
| :--- | :--- | :--- | :--- |
| **`functions/`** | `bounds_to_range.stan` | `test-stan-bounds_to_range.R` | Index segment calculation and validation for cumulative weight bounds |
| | `layer_offsets.stan` | `test-stan-layer_offsets.R` | Multi-layer tree offset accumulation and hierarchical phi calculation |
| | `lookups.stan` | `test-stan-lookups.R` | Column-major index flattening (`compute_cdf_lookup`, `compute_phi_lookup`) and bounds validation |
| | `unrolled_dose_static_lambda.stan` | `test-stan-unrolled_dose.R` | Multi-dose CDF unrolling given schedule and rate $\lambda$ |
| | `convenience.stan` | *(composite include)* | Tested via constituent sub-function unit tests |
| **`data/`** | `uncensored/`, `right/`, `left/` | *(composite includes)* | Modular observation data and weights definitions |
| | `locations.stan`, `structural.stan` | *(composite includes)* | Structural indices and location hierarchy data |
| **`transformed_data/`** | `common_indices.stan`, `layer_phi_lookup.stan` | `test-stan-common_indices.R` | Structural integration for precomputed indices (`obs_map_*`, `cdf_lookup_*`, `phi_lookup_*`) |
| | `layer_indices.stan` | `test-stan-layer_indices.R` | Multi-layer location bounds: `layer_bounds`, `parent_child_bounds`, `loc_layer_idx` |
| | `single_phi_lookup.stan` | `test-stan-single_phi_lookup.R` | Single-location phi lookups (`phi_lookup_*`) via subdirectories |
| **`model/`** | `hierarchical_phi.stan` | `test-stan-hierarchical_phi.R` | Deterministic hierarchical observation probabilities against analytical formula |
| | `single_phi.stan` | `test-stan-single_phi.R` | Deterministic single-location observation probabilities against analytical formula |
| | `observation_likelihood.stan` | `test-stan-observation_likelihood.R` | Modular observation log-likelihoods (`uncensored/`, `right/`, `left/`) |

---

## Error Messages, Signaling Standards, and Unit Testing

All user-facing validation errors and warnings should follow these standards:

### 1. Centralized Named Template Constants
* Define error and warning message format strings as constants at the top of each R file
  prefixed with `ERR_` or `MSG_`:
* Use named `{var}` placeholders (e.g. `{dose}`, `{sched_age}`, `{n_doses}`) rather than cryptic
  unnamed format specifiers to keep message definitions self-documenting:
  ```r
  ERR_POP_DOSE_INCOMPATIBLE <- paste0(
    "dose {dose} requires age > {sched_age} (`dose_schedule[{dose}] == {sched_age}`), but ",
    "`populations` contains observations where all ages are <= {sched_age}; ",
    "use `subset(populations, dose == {dose} & age <= {sched_age})` or configure ",
    "`imugap_options(dose_schedule = ...)` to resolve invalid entries"
  )
  ERR_OPT_UNKNOWN_MODEL <- "`imugap_opts` unknown model '{model}'"
  ```

### 2. Signaling Functions: `stop_fmt_if` and `warn_fmt_if`

* Use internal helpers `stop_fmt_if()` and `warn_fmt_if()` for validation assertions, passing named
  arguments matching the `{var}` placeholders in the template:

  ```r
  stop_fmt_if(
    length(invalid_obs) > 0L,
    ERR_POP_DOSE_INCOMPATIBLE,
    dose = k,
    sched_age = dose_schedule[k]
  )
  ```

* Use the parameter `n` to adjust the call stack offset so the error is attributed to the user's
  top-level function call rather than internal helper functions.

### 3. Error Message Unit Testing via `err_pattern`

* In unit tests (`tests/testthat/`), verify error and warning messages using the test helper
  `err_pattern(ERR_..., ...)`:
  ```r
  expect_error(
    validate_dose_schedule(c(1L, 4L), wts_unmixed),
    err_pattern(ERR_POP_DOSE_INCOMPATIBLE, dose = 2L, sched_age = 4L)
  )
  ```
* **Wildcards and Partial Matches**: `err_pattern()` escapes all regex metacharacters in literal
  template text and substitutes specified slot values. Unsupplied or `NA`/`NULL` slots automatically
  match wildcards (`.+?`), allowing unit tests to assert on key parameter values while remaining
  resilient against minor phrasing changes.

### 4. Typography: Backticks vs. Single Quotes

Follow a strict convention when formatting error and warning strings:

* **Backticks (`` `code` ``)**: Use for formal R code symbols, argument names, function names,
  expressions, and classes:
  * `` `observations` must be a data.frame ``
  * `` `df` must be a single positive integer ``
  * `` `stan_opts` must be created by stan_options() ``

* **Single Quotes (`'value'`)**: Use for user-supplied string values, column names, model names, or
  discrete inputs:
  * `` column '{col}' cannot contain NA values ``
  * `` unknown model '{model}' ``
  * `` '{arg}' must be numeric ``

### 4. Markdown & Vignette Text Formatting Standards

To ensure clean rendering across GitHub, `pkgdown`, and Pandoc HTML/PDF engines:

* **No Hard Line Breaks in Prose Text**: Do not insert line breaks when writing or managing prose text in vignettes (`vignettes/*.Rmd`). Each paragraph and list item description should remain on a single unbroken line.
* **Preceding Blank Lines**: Always separate preceding introductory text from lists with an empty blank line (`\n\n`). Never start a list immediately on the line following a colon or text.
* **Consistent Indentation & Sub-Lists**: Indent sub-lists by 2 or 4 spaces and use consistent bullet styling (`-`). Avoid mixing unindented numbered sequences under unordered list items.
* **Multi-Line Continuation Margin**: When list items span multiple lines, align continuation lines with the item text margin (e.g. 2 spaces for `- `, 3 spaces for `1. `).

---

## Package Dependencies and Stan Backend

### 1. Dependency Management and Minimization

* **Avoid Unnecessary Dependencies**: Prefer base R or existing declared dependencies
  (`data.table`, `testthat`, etc.) rather than introducing new external dependencies. Avoid adding
  convenience libraries (e.g. `rprojroot`, `fs`, `glue`) when existing base R constructs or already
  imported tools achieve the same result.
* **Adding Warranted Dependencies via `usethis`**: When a new dependency is genuinely warranted
  (e.g., required for a core feature, an optional backend, or specialized vignette computation),
  always record it in `DESCRIPTION` using `usethis::use_package()` (e.g.
  `usethis::use_package("pkgname", type = "Imports")` or
  `usethis::use_package("pkgname", type = "Suggests")`). Using `usethis` tooling ensures
  consistent schema compliance, proper field classification, and alphabetical ordering.

### 2. Stan Backend and Pinning

* **`flexstanr`**: Portable Stan backend support is provided by the imported package
  `flexstanr (>= 0.2.0)`. The integration helper `R/flexstanr.R` is generated by
  `flexstanr::use_flexstanr()`.
* **`cmdstanr`**: An optional, non-CRAN `Suggests`. It is resolved in CI via
  **`Remotes: stan-dev/cmdstanr`** in `DESCRIPTION`.
* **Stan Stack Pinning**: **Do not** add the `stan-dev` r-universe as an extra repository in CI
  workflows: `pak` would then select dev builds of `StanHeaders`/`rstan`, which fail to compile
  against CRAN's `RcppEigen` (#101). `Remotes` pins *only* `cmdstanr` while keeping the remainder
  of the Stan stack on CRAN.

---

## Pull Request and CI Workflows

Every pull request triggers automated GitHub Actions workflows that delegate directly to `justfile` recipes for build, documentation, and validation:

1. **`R-CMD-check`**: Runs `R CMD check --as-cran` across Ubuntu, macOS, and Windows on R release, oldrel, and devel (9 jobs), using shared build artifacts from `just data-fit`, `just diagrams`, and `just docs`.
2. **`lint`**: Runs `just lint` (verifying `air format . --check` and `lintr::lint_package()`).
3. **`test-coverage`**: Computes code coverage with `covr` and uploads results to Codecov.
4. **`pkgdown`**: Builds the documentation site and confirms that all vignettes compile cleanly. Deployed to GitHub Pages upon push to `main` and published releases.

All checks must pass before merging.
