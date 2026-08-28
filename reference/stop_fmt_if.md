# Signal an error if a condition is met with formatted message

Signal an error if a condition is met with formatted message

## Usage

``` r
stop_fmt_if(cond, fmt, ..., n = 1L)
```

## Arguments

- cond:

  Logical expression to evaluate.

- fmt:

  Character format string for
  [`sprintf()`](https://rdrr.io/r/base/sprintf.html).

- ...:

  Additional arguments passed to
  [`sprintf()`](https://rdrr.io/r/base/sprintf.html).

- n:

  Frame offset integer specifying call stack depth for call attribution
  (default `1L`). If `n <= 0L`, call attribution is suppressed (`NULL`).
