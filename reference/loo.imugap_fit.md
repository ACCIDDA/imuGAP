# Leave-one-out cross-validation for `imugap_fit`

Computes approximate leave-one-out cross-validation (LOO-CV) using
Pareto smoothed importance sampling (PSIS-LOO) via the `{loo}` package.

## Usage

``` r
# S3 method for class 'imugap_fit'
loo(x, posterior_size = NULL, ...)
```

## Arguments

- x:

  an `imugap_fit` object returned by
  [`sampling()`](https://accidda.github.io/imuGAP/reference/sampling.md).

- posterior_size:

  optional integer scalar; how many draws to use from the end of each
  chain? (default: `NULL`, which uses all draws).

- ...:

  additional arguments passed to
  [`loo::loo()`](https://mc-stan.org/loo/reference/loo.html) (e.g.
  `cores`, `r_eff`). When `r_eff` is omitted and the fit contains
  multiple chains, relative effective sample size is automatically
  calculated via
  [`loo::relative_eff()`](https://mc-stan.org/loo/reference/relative_eff.html).

## Value

for `loo.imugap_fit()`: an object of class `loo`, as returned by
[`loo::loo()`](https://mc-stan.org/loo/reference/loo.html).

## Examples

``` r
if (FALSE) { # interactive() && requireNamespace("loo", quietly = TRUE)
# \donttest{
data("fit_sim", package = "imuGAP")
loo_res <- loo::loo(fit_sim, posterior_size = 100)
print(loo_res)
# }
}
```
