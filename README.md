# imuGAP <img src="man/figures/logo.png" align="right" height="139" alt="imuGAP logo" />

[![CRAN status](https://www.r-pkg.org/badges/version/imuGAP)](https://CRAN.R-project.org/package=imuGAP) [![codecov](https://codecov.io/gh/ACCIDDA/imuGAP/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ACCIDDA/imuGAP) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.20562250.svg)](https://doi.org/10.5281/zenodo.20562250)

`{imuGAP}`, which stands for "Immunity: Geographic & Age-based Projection", provides a fitting and imputation / prediction tool for a process-based model of vaccination uptake.

## Installation

Install the released version from CRAN:

```r
install.packages("imuGAP")
```

Or install the development version from GitHub:

```r
remotes::install_github("ACCIDDA/imuGAP")
```

Requires R >= 4.1.0 and a C++ toolchain (for Stan model compilation).

## Model

The `{imuGAP}` sampler fits two model effects: a lifetime tendency towards vaccination and a time varying force of vaccination. To fit those effects, the model evaluates observations which may combine many populations. The model distinguishes populations by place, birth timing, and age.

The model organizes places into a hierarchy, for example a state which contains counties which in turn contain cities. The effects in a particular place combine the impact of all enclosing locations.

