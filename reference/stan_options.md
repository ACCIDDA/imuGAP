# Stan Sampler Options

This function encapsulates option passing to the stan sampler, with the
exception of the model object, which is passed in `imugap_options`.

## Usage

``` r
stan_options(...)
```

## Arguments

- ...:

  Arguments passed on to
  [`rstan::sampling`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)

  :   

## Value

a list of arguments matching
[`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
inputs

## Examples

``` r
stan_options()
#> list()
stan_options(chains = 2, iter = 500)
#> $chains
#> [1] 2
#> 
#> $iter
#> [1] 500
#> 
```
