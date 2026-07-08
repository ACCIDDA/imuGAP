# Create observation populations

`create_observation_populations` is a convenience function to construct
a properly weighted `populations` object for typical modes of
observation.

## Usage

``` r
create_observation_populations(observations, mode = "snapshot", ...)
```

## Arguments

- observations:

  a pre- or post-canonicalization `observations` object. Optionally
  contains additional columns required for the specified `mode` that
  vary by row.

- mode:

  character; the mode for populations creation (default: "snapshot").

- ...:

  additional arguments determined by the specified `mode` requirements
  for values which do not vary by row.

## Value

A `data.table` representing the populations mapping.

## Details

This function uses a combination of varying information from
`observations` and fixed information from `...` arguments to provide the
necessary information for the modes to produce a
`[canonicalize_populations()]` ready-object.

Supported modes and required information:

## "snapshot" mode

As with `[create_target()]`, a snapshot view is looking at a particular
place, time, and dose target, but with varying birth cohorts. That means
the sum of birth cohort and age is constant: if birth cohort 1 is age
10, then cohort 2 is 9, and so on.

Snapshots requires `obs_id`, `loc_id`, `dose`, `age_min`, and `cohort`
the reference cohort corresponding to the oldest age. `age_max` may be
provided, but if missing or `NA`, is assumed to be `age_min` + 1.
`age_max` corresponds to the first *excluded* age - i.e.

\$\$ age\[age_min,age_max) \$\$

Taking this approach to `age_max` enables this method to naturally
support partial cohorts. For example, if `age_max = 18.5` and
`age_min = 17`, then age 17 population has 2/3rds the weight and the age
18 population has 1/3rd. `age_min` works the same way.

Note that "snapshot" mode assumes that all populations are uniformly
sized with respect to weighting. This assumption may be inadequate when
population age groups contributing to an observation are very
differently sized.
