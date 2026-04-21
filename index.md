# imuGAP

[imuGAP](https://accidda.github.io/imuGAP/) provides a fitting and
imputation / prediction tool for a particular process model of
vaccination uptake. The tool provides flexible trends and relationships
for the elements of the process.

## Quick Start

``` r
remotes::install_github("ACCIDDA/imuGAP")
```

## Model

The [imuGAP](https://accidda.github.io/imuGAP/) sampler fits two model
effects: a lifetime tendency towards vaccination and a time varying
force of vaccination. To fit those effects, the model evaluates
observations which may combine many populations. The model distinguishes
populations by place, birth timing, and age.

The model organizes places into a hierarchy, for example a state which
contains counties which in turn contain cities. The effects in a
particular place combine the impact of all enclosing locations.

## Installation

``` r
# Install from GitHub
remotes::install_github("ACCIDDA/imuGAP")
```

Requires R \>= 4.1.0 and a C++ toolchain (for Stan model compilation).

## CLI Usage

After installing the package, set up the command-line interface:

``` r
imuGAP::install_cli()
```

This creates a symlink at `~/.local/bin/imugap`. Make sure
`~/.local/bin` is on your `PATH`.

You only need to run
[`install_cli()`](https://accidda.github.io/imuGAP/reference/install_cli.md)
once after installing or updating the package; it refreshes the symlink
so it points at the current install.

### Commands

``` bash
# Show help
imugap -h

# Validate input data (dry run, no model fitting)
imugap -h <input_dir>

# Run model fitting
imugap <input_dir> [output_dir]
```

`output_dir` defaults to `input_dir` if not specified. Output is a
`fit.rds` file containing the raw `stanfit` object.

### Input Files

`input_dir` must contain three files (CSV or RDS):

| File           | Columns                                               | Description                                                                                 |
|----------------|-------------------------------------------------------|---------------------------------------------------------------------------------------------|
| `observations` | `obs_id`, `positive`, `sample_n`                      | School-level vaccination counts.                                                            |
| `populations`  | `obs_id`, `loc_id`, `cohort`, `age`, `dose`, `weight` | Observation metadata. Weights must sum to 1 per `obs_id`. `dose` must include both 1 and 2. |
| `locations`    | `loc_id`, `parent_id`                                 | Location hierarchy (state \> county \> school). Root has `parent_id = NA`.                  |

### Exit Codes

| Code | Meaning          |
|------|------------------|
| 0    | Success          |
| 1    | Validation error |
| 2    | Model error      |
| 3    | I/O error        |
