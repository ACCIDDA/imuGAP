# Assert a backend name is valid and its package is installed

Validates `backend` against the known choices (so it also subsumes
[`match.arg()`](https://rdrr.io/r/base/match.arg.html)) and, for the
optional cmdstanr backend, that its package is installed. rstan is
always available (a hard dependency); cmdstanr is optional, so selecting
it without the package installed fails early here rather than deep
inside the fit. Returns the validated backend invisibly.

## Usage

``` r
assert_backend_available(backend)
```

## Arguments

- backend:

  the backend to validate.

## Value

the validated backend string, invisibly.
