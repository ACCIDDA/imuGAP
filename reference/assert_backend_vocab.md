# Assert that no foreign-backend argument vocabulary was used

Errors if any argument name belongs to the *other* backend's vocabulary,
with a "did you mean" hint. On success returns the argument names
invisibly.

## Usage

``` r
assert_backend_vocab(arg_names, backend)
```

## Arguments

- arg_names:

  names of the arguments supplied to
  [`stan_options()`](https://accidda.github.io/imuGAP/reference/stan_options.md).

- backend:

  the backend the options are being built for.

## Value

`arg_names`, invisibly.
