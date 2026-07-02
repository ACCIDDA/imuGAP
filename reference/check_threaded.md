# Check whether per-chain threading is enabled for the active backend

cmdstanr configures threading through the `threads_per_chain` sampler
argument; rstan reads the `STAN_NUM_THREADS` environment variable at run
time (`-1` meaning all available cores). Only the run-time configuration
is checked, not whether the model was compiled with threading support.
The fit functions do not consult this themselves: a host package running
a threaded model calls it to warn when the user has not made threads
available.

## Usage

``` r
check_threaded(stan_opts)
```

## Arguments

- stan_opts:

  a
  [`stan_options()`](https://accidda.github.io/imuGAP/reference/stan_options.md)
  result.

## Value

logical; `TRUE` if per-chain threading is enabled, otherwise `FALSE`.
