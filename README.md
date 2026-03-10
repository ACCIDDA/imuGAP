# imuGAP

`{imuGAP}` provides a fitting and imputation / prediction tool for a particular process model of vaccination uptake. The tool provides flexible trends and relationships for the elements of the process.

Briefly, the process model consists of two parts: a lifetime tendency towards vaccination and a time varying force of vaccination. Those effects occur in a nested setting - a largest enclosing population, with mutually exclusive internal populations down to a lowest resolution level.

## Installation

```r
# Install from GitHub
remotes::install_github("ACCIDDA/imuGAP")
```

Requires R >= 3.4.0 and a C++ toolchain (for Stan model compilation).

## CLI Usage

After installing the package, set up the command-line interface:

```r
imuGAP::install_cli()
```

This creates a symlink at `~/.local/bin/imugap`. Make sure `~/.local/bin` is on your `PATH`.

### Commands

```bash
# Show help
imugap -h

# Validate input data (dry run, no model fitting)
imugap -h <input_dir>

# Run model fitting
imugap <input_dir> [output_dir]
```

`output_dir` defaults to `input_dir` if not specified. Output is a `fit.rds` file containing the raw `stanfit` object.

### Input Files

`input_dir` must contain three files (CSV or RDS):

| File | Columns | Description |
|------|---------|-------------|
| `observations` | `positive`, `sample_n` | School-level vaccination counts. Optional `id` column (1:N). |
| `obs_populations` | `obs_id`, `location`, `cohort`, `age`, `dose`, `weight` | Observation metadata. Weights must sum to 1 per `obs_id`. `dose` must include both 1 and 2. |
| `locations` | `id`, `parent_id` | Location hierarchy (state > county > school). Root has `parent_id = NA`. |

### Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | Validation error |
| 2 | Model error |
| 3 | I/O error |

