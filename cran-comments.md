## Submission Notes for imuGAP 0.2.0

This is a minor release update of imuGAP (version 0.2.0).

In response to

"
Thanks, we see:

   Unknown, possibly misspelled, fields in DESCRIPTION:
     'Remotes'

Please fix and resubmit.
"

We have removed the remotes field and adjusted our internal CI. `cmdstanr` continues to appear in the `Suggests` field as an optional non-CRAN package that users may want to use with `imuGAP`. To support discoverability for internal CI with `pak`, we added

```
Config/pak/dependencies:
    stan-dev/cmdstanr
```

### Summary of Changes Since Previous Version (0.1.0)

* **Arbitrary Hierarchy Layer Depth**:
  - `sampling()`, `canonicalize_locations()`, and underlying Stan models now support user-defined hierarchical location partitions of arbitrary depth (1-layer statewide, 2-layer state-county, 3-layer state-county-school, or deeper regional partitions).
  - Added `assemble_layer_data()` helper to automatically construct and validate hierarchy metadata and parent mappings.
  - Added dedicated single-layer model `impute_school_coverage_process_v6_single_layer` with automatic dispatch.
  - Added a new vignette (`user_specified_layers`) demonstrating 1-layer, 2-layer, and 3-layer modeling workflows.
* **Backend Migration to `flexstanr`**:
  - Migrated backend abstraction to the CRAN package `flexstanr (>= 0.2.0)`, providing seamless interoperability across `rstan` and `cmdstanr`.
* **Modular Stan Architecture**:
  - Refactored top-level Stan models into concise assembly skeletons utilizing modular `#include` directives (`functions/`, `data/`, `transformed_data/`, `parameters/`, `model/`).
  - Pruned vestigial Stan trial scripts.
* **Data Pipeline & Target Generation**:
  - Split target generation into fit-free constructor `create_target()` and validator `canonicalize_target()`.
  - Added bundled example fitted datasets for single-layer and 2-layer models (`fit_sim_1layer`, `fit_sim_2layer`, `predict_sim_1layer`, `predict_sim_2layer`, `target_sim_1layer`, `target_sim_2layer`).
* **Validation & Error Handling**:
  - Enforced structural location invariant (nodes have either 0 or >= 2 offspring).
  - Standardized assertion messages with module-level `ERR_*` format string constants.

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
