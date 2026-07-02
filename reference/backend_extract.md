# Extract named parameters from a fit as a list of arrays

Matches the shape returned by
[`rstan::extract()`](https://mc-stan.org/rstan/reference/stanfit-method-extract.html).

## Usage

``` r
backend_extract(raw_fit, pars, ...)
```

## Arguments

- raw_fit:

  a backend-native fit object.

- pars:

  character vector of parameter names to extract.

- ...:

  forwarded to the backend's extractor.

## Value

a named list of draw arrays, one per parameter.
