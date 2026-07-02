# Run generated quantities against a fit and return a parameter matrix

Run generated quantities against a fit and return a parameter matrix

## Usage

``` r
backend_generate_quantities(raw_fit, data, draws_mat, pars)
```

## Arguments

- raw_fit:

  a backend-native fit object.

- data:

  the Stan data list for the generated-quantities run.

- draws_mat:

  a draws matrix (rows = draws, columns = parameters).

- pars:

  name of the generated parameter to return.

## Value

a matrix of the requested generated parameter (rows = draws).
