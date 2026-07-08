# Example Coverage Predictions

A dataset containing predicted vaccine coverage probabilities generated
by calling [`predict()`](https://rdrr.io/r/stats/predict.html) on
`fit_sim` with `target_sim` as the target.

## Format

An object of class `imugap_predict` wrapping:

- `draws`, a 3D array of predicted draws with dimensions
  `[iteration, chain, variable]`

- `target`, a `[data.table()]` containing target population parameters
  matching the variables
