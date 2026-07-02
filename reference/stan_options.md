# Stan Sampler Options

Collects and validates sampler arguments for the chosen `backend`,
forwarding them **verbatim** so calls feel native to that backend. Use
the backend's own argument names; mixing one backend's vocabulary into
the other errors with a hint. The model object is supplied separately
via the package's model options, while `data` and `init` are constructed
internally, so none of these may be set here. `chains` defaults to `4`
so downstream code can always size per-chain structures from it.

## Usage

``` r
stan_options(..., chains = 4L, backend = "rstan")
```

## Arguments

- ...:

  sampler arguments forwarded verbatim to the chosen backend's sampler.
  Use the backend's own names: for `"rstan"`, the
  [`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
  arguments (`iter`, `cores`, `seed`); for `"cmdstanr"`, the `$sample()`
  arguments (`iter_warmup`, `iter_sampling`, `parallel_chains`, ...).

- chains:

  A positive integer specifying the number of Markov chains. The default
  is 4.

- backend:

  which Stan interface to target, one of `"rstan"` (default) or
  `"cmdstanr"`. Determines which argument vocabulary is accepted and
  which sampler
  [`sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
  calls. Selecting `"cmdstanr"` errors if the cmdstanr package is not
  installed.

## Value

a named list of validated sampler arguments, carrying a `backend`
element recording the backend it was built for

## Examples

``` r
stan_options()
#> $chains
#> [1] 4
#> 
#> $backend
#> [1] "rstan"
#> 
stan_options(chains = 2, iter = 500)
#> $iter
#> [1] 500
#> 
#> $chains
#> [1] 2
#> 
#> $backend
#> [1] "rstan"
#> 
if (requireNamespace("cmdstanr", quietly = TRUE)) {
  stan_options(backend = "cmdstanr", parallel_chains = 4, iter_warmup = 500)
}
#> $parallel_chains
#> [1] 4
#> 
#> $iter_warmup
#> [1] 500
#> 
#> $chains
#> [1] 4
#> 
#> $backend
#> [1] "cmdstanr"
#> 
```
