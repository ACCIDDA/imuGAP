# Example Coverage Predictions

A dataset containing predicted vaccine coverage probabilities generated
by calling [`predict()`](https://rdrr.io/r/stats/predict.html) on
`fit_sim` with `target_sim` as the target.

## Usage

``` r
predict_sim
```

## Format

A `[data.table()]` with 50400 rows and 9 columns:

- `sample_id`, a number, the posterior MCMC sample index/ID

- `p_obs`, a number, the predicted vaccine coverage probability

- `loc_id`, a string, the location ID

- `age`, a number, the age

- `cohort`, a number, the birth cohort

- `dose`, a number, the vaccine dose (1 or 2)

- `weight`, a number, the weight of the prediction component

- `loc_c_id`, a number, the compiled location ID

- `obs_id`, a number, the target observation ID matching `obs_c_id` in
  `target_sim`
