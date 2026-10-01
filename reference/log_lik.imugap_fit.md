# Pointwise log-likelihood matrix for `imugap_fit`

Computes the pointwise log-likelihood matrix for an `imugap_fit` object
across all observations and posterior draws.

## Usage

``` r
# S3 method for class 'imugap_fit'
log_lik(object, posterior_size = NULL, ...)
```

## Arguments

- object:

  an `imugap_fit` object returned by
  [`sampling()`](https://accidda.github.io/imuGAP/reference/sampling.md).

- posterior_size:

  optional integer scalar; how many draws to use from the end of each
  chain? (default: `NULL`, which uses all draws).

- ...:

  additional arguments (currently ignored).

## Value

for `log_lik.imugap_fit()`: a numeric matrix of dimensions `S x N`,
where `S` is the number of posterior draws and `N` is the total number
of observations in the fit.

## Examples

``` r
if (FALSE) { # interactive()
# \donttest{
data("fit_sim", package = "imuGAP")
ll <- rstantools::log_lik(fit_sim, posterior_size = 50)
dim(ll)
# }
}
```
