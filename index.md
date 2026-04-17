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
