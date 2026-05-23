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

  `data`

  :   A named `list` or `environment` providing the data for the model
      or a character vector for all the names of objects used as data.
      See the **Passing data to Stan** section in
      [`stan`](https://mc-stan.org/rstan/reference/stan.html).

  `pars`

  :   A vector of character strings specifying parameters of interest.
      The default is `NA` indicating all parameters in the model. If
      `include = TRUE`, only samples for parameters named in `pars` are
      stored in the fitted results. Conversely, if `include = FALSE`,
      samples for all parameters *except* those named in `pars` are
      stored in the fitted results.

  `chains`

  :   A positive integer specifying the number of Markov chains. The
      default is 4.

  `iter`

  :   A positive integer specifying the number of iterations for each
      chain (including warmup). The default is 2000.

  `warmup`

  :   A positive integer specifying the number of warmup (aka burnin)
      iterations per chain. If step-size adaptation is on (which it is
      by default), this also controls the number of iterations for which
      adaptation is run (and hence these warmup samples should not be
      used for inference). The number of warmup iterations should be
      smaller than `iter` and the default is `iter/2`.

  `thin`

  :   A positive integer specifying the period for saving samples. The
      default is 1, which is usually the recommended value.

  `seed`

  :   The seed for random number generation. The default is generated
      from 1 to the maximum integer supported by R on the machine. Even
      if multiple chains are used, only one seed is needed, with other
      chains having seeds derived from that of the first chain to avoid
      dependent samples. When a seed is specified by a number,
      `as.integer` will be applied to it. If `as.integer` produces `NA`,
      the seed is generated randomly. The seed can also be specified as
      a character string of digits, such as `"12345"`, which is
      converted to integer.

  `init`

  :   Initial values specification. See the detailed documentation for
      the init argument in
      [`stan`](https://mc-stan.org/rstan/reference/stan.html).

  `check_data`

  :   Logical, defaulting to `TRUE`. If `TRUE` the data will be
      preprocessed; otherwise not. See the **Passing data to Stan**
      section in
      [`stan`](https://mc-stan.org/rstan/reference/stan.html).

  `sample_file`

  :   An optional character string providing the name of a file. If
      specified the draws for *all* parameters and other saved
      quantities will be written to the file. If not provided, files are
      not created. When the folder specified is not writable,
      [`tempdir()`](https://rdrr.io/r/base/tempfile.html) is used. When
      there are multiple chains, an underscore and chain number are
      appended to the file name prior to the `.csv` extension.

  `diagnostic_file`

  :   An optional character string providing the name of a file. If
      specified the diagnostics data for *all* parameters will be
      written to the file. If not provided, files are not created. When
      the folder specified is not writable,
      [`tempdir()`](https://rdrr.io/r/base/tempfile.html) is used. When
      there are multiple chains, an underscore and chain number are
      appended to the file name prior to the `.csv` extension.

  `verbose`

  :   `TRUE` or `FALSE`: flag indicating whether to print intermediate
      output from Stan on the console, which might be helpful for model
      debugging.

  `algorithm`

  :   One of sampling algorithms that are implemented in Stan. Current
      options are `"NUTS"` (No-U-Turn sampler, Hoffman and Gelman 2011,
      Betancourt 2017), `"HMC"` (static HMC), or `"Fixed_param"`. The
      default and preferred algorithm is `"NUTS"`.

  `control`

  :   A named `list` of parameters to control the sampler's behavior.
      See the details in the documentation for the `control` argument in
      [`stan`](https://mc-stan.org/rstan/reference/stan.html).

  `include`

  :   Logical scalar defaulting to `TRUE` indicating whether to include
      or exclude the parameters given by the `pars` argument. If
      `FALSE`, only entire multidimensional parameters can be excluded,
      rather than particular elements of them.

  `cores`

  :   Number of cores to use when executing the chains in parallel,
      which defaults to 1 but we recommend setting the `mc.cores` option
      to be as many processors as the hardware and RAM allow (up to the
      number of chains).

  `open_progress`

  :   Logical scalar that only takes effect if `cores > 1` but is
      recommended to be `TRUE` in interactive use so that the progress
      of the chains will be redirected to a file that is automatically
      opened for inspection. For very short runs, the user might prefer
      `FALSE`.

  `show_messages`

  :   Either a logical scalar (defaulting to `TRUE`) indicating whether
      to print the summary of Informational Messages to the screen after
      a chain is finished or a character string naming a path where the
      summary is stored. Setting to `FALSE` is not recommended unless
      you are very sure that the model is correct up to numerical error.

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
