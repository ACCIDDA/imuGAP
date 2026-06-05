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
#> 1004:   1004 0.9136193 0.8900935 0.9137017 0.9356396
#> 1005:   1005 0.9675009 0.9457533 0.9677305 0.9873141
#> 1006:   1006 0.8571546 0.8392127 0.8569275 0.8755120
#> 1007:   1007 0.9212400 0.9035732 0.9211019 0.9392955
#> 1008:   1008 0.9757615 0.9593055 0.9758310 0.9912522

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
#> 1004:   1004 0.9136193 0.8986274 0.9137017 0.9285095
#> 1005:   1005 0.9675009 0.9537224 0.9677305 0.9807911
#> 1006:   1006 0.8571546 0.8450981 0.8569275 0.8690846
#> 1007:   1007 0.9212400 0.9093046 0.9211019 0.9326662
#> 1008:   1008 0.9757615 0.9655694 0.9758310 0.9864589
```
