# Test Guide

This folder includes a smoke test that quickly validates core input-processing
paths for `imuGAP` using both CSV fixtures and package data.

## What the smoke test checks

- Observation schema validation (`check_observations`)
- Location hierarchy validation (`check_locations`)
- Observation-population mapping validation (`check_obs_population`)
- Cross-file consistency between school IDs and location IDs

The full Stan model fit call is intentionally skipped in the smoke suite.

## Required R libraries

Install these packages first:

```r
install.packages(c("testthat", "data.table"), repos = "https://cloud.r-project.org")
```

## Run the smoke test

From the repository root:

```bash
export PATH="/opt/homebrew/bin:$PATH"
Rscript -e 'library(testthat); library(data.table); test_dir("tests/testthat", filter = "smoke")'
```

If your shell already has Homebrew on `PATH`, you can skip the `export PATH=...`
line.

## Test files and data

- Smoke test file: `tests/testthat/test-smoke.R`
- CSV fixtures: `tests/testthat/testdata/`
- Package data fixtures: `data/observations_sim.rda`, `data/obs_populations_sim.rda`, `data/locations_sim.rda`

