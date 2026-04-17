# Custom imuGAP fit extraction

Thin wrapper around
[`rstan::extract`](https://mc-stan.org/rstan/reference/stanfit-method-extract.html)
to extract typical imuGAP parameters.

## Usage

``` r
extract_imugap(fit, pars = c("logit_phi_state"), ...)
```

## Arguments

- fit:

  a `stanfit` object returned by `imuGAP()`

- pars:

  character vector; parameters to extract.

## Value

a list, as returned by
[`rstan::extract()`](https://mc-stan.org/rstan/reference/stanfit-method-extract.html)
