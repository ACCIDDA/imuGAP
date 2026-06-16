# Summarize coverage predictions

Summarizes predicted coverage probabilities from an `imugap_predict`
object by location, cohort, age, and dose for the requested quantiles.

## Usage

``` r
# S3 method for class 'imugap_predict'
summary(object, probs = c(0.025, 0.5, 0.975), ...)
```

## Arguments

- object:

  an `imugap_predict` object returned by `[predict()]`

- probs:

  numeric vector of probabilities/quantiles to compute. Defaults to
  `c(0.025, 0.5, 0.975)`.

- ...:

  additional arguments (currently ignored)

## Value

A `data.table` containing target population parameters, posterior mean
coverage (`mean`), and the requested quantiles (e.g. `q2.5`, `q50`,
`q97.5`).

## Examples

``` r
# Load example prediction object
data("predict_sim", package = "imuGAP")

# Summarize coverage predictions
summary(predict_sim)
#>       obs_c_id                  loc_id   age cohort  dose weight loc_c_id
#>          <int>                  <char> <int>  <num> <num>  <num>    <int>
#>    1:        1                   State     1     30     1      1        1
#>    2:        2                 Scruggs     1     30     1      1        2
#>    3:        3                  Simone     1     30     1      1        3
#>    4:        4                  Watson     1     30     1      1        4
#>    5:        5    Chickadee Elementary     1     30     1      1        8
#>   ---                                                                    
#> 1004:     1004     Mockingbird Academy    18     13     2      1       27
#> 1005:     1005 Kinglet Learning Center    18     13     2      1       25
#> 1006:     1006            Vireo School    18     13     2      1       28
#> 1007:     1007      Kingfisher Academy    18     13     2      1       24
#> 1008:     1008    Cormorant Elementary    18     13     2      1       22
#>       obs_id      mean      q2_5       q50     q97_5
#>        <int>     <num>     <num>     <num>     <num>
#>    1:      1 0.0000000 0.0000000 0.0000000 0.0000000
#>    2:      2 0.0000000 0.0000000 0.0000000 0.0000000
#>    3:      3 0.0000000 0.0000000 0.0000000 0.0000000
#>    4:      4 0.0000000 0.0000000 0.0000000 0.0000000
#>    5:      5 0.0000000 0.0000000 0.0000000 0.0000000
#>   ---                                               
#> 1004:   1004 0.9143331 0.8950789 0.9147863 0.9324231
#> 1005:   1005 0.9690266 0.9460750 0.9699277 0.9870447
#> 1006:   1006 0.8590557 0.8420708 0.8584125 0.8760457
#> 1007:   1007 0.9222259 0.9046294 0.9210967 0.9384568
#> 1008:   1008 0.9773036 0.9633049 0.9788489 0.9912069

# Summarize with custom quantiles
summary(predict_sim, probs = c(0.1, 0.5, 0.9))
#>       obs_c_id                  loc_id   age cohort  dose weight loc_c_id
#>          <int>                  <char> <int>  <num> <num>  <num>    <int>
#>    1:        1                   State     1     30     1      1        1
#>    2:        2                 Scruggs     1     30     1      1        2
#>    3:        3                  Simone     1     30     1      1        3
#>    4:        4                  Watson     1     30     1      1        4
#>    5:        5    Chickadee Elementary     1     30     1      1        8
#>   ---                                                                    
#> 1004:     1004     Mockingbird Academy    18     13     2      1       27
#> 1005:     1005 Kinglet Learning Center    18     13     2      1       25
#> 1006:     1006            Vireo School    18     13     2      1       28
#> 1007:     1007      Kingfisher Academy    18     13     2      1       24
#> 1008:     1008    Cormorant Elementary    18     13     2      1       22
#>       obs_id      mean       q10       q50       q90
#>        <int>     <num>     <num>     <num>     <num>
#>    1:      1 0.0000000 0.0000000 0.0000000 0.0000000
#>    2:      2 0.0000000 0.0000000 0.0000000 0.0000000
#>    3:      3 0.0000000 0.0000000 0.0000000 0.0000000
#>    4:      4 0.0000000 0.0000000 0.0000000 0.0000000
#>    5:      5 0.0000000 0.0000000 0.0000000 0.0000000
#>   ---                                               
#> 1004:   1004 0.9143331 0.9020982 0.9147863 0.9281548
#> 1005:   1005 0.9690266 0.9559556 0.9699277 0.9825058
#> 1006:   1006 0.8590557 0.8505981 0.8584125 0.8706470
#> 1007:   1007 0.9222259 0.9094963 0.9210967 0.9334922
#> 1008:   1008 0.9773036 0.9662763 0.9788489 0.9864124
```
