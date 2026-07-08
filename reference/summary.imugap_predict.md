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
#>            mean      q2_5       q50     q97_5
#>           <num>     <num>     <num>     <num>
#>    1: 0.0000000 0.0000000 0.0000000 0.0000000
#>    2: 0.0000000 0.0000000 0.0000000 0.0000000
#>    3: 0.0000000 0.0000000 0.0000000 0.0000000
#>    4: 0.0000000 0.0000000 0.0000000 0.0000000
#>    5: 0.0000000 0.0000000 0.0000000 0.0000000
#>   ---                                        
#> 1004: 0.8964275 0.8800058 0.8963465 0.9116182
#> 1005: 0.9273869 0.9101226 0.9275322 0.9439182
#> 1006: 0.8391046 0.8152567 0.8399715 0.8635837
#> 1007: 0.8908591 0.8661748 0.8910117 0.9113518
#> 1008: 0.9545076 0.9391573 0.9560450 0.9693223

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
#>            mean       q10       q50       q90
#>           <num>     <num>     <num>     <num>
#>    1: 0.0000000 0.0000000 0.0000000 0.0000000
#>    2: 0.0000000 0.0000000 0.0000000 0.0000000
#>    3: 0.0000000 0.0000000 0.0000000 0.0000000
#>    4: 0.0000000 0.0000000 0.0000000 0.0000000
#>    5: 0.0000000 0.0000000 0.0000000 0.0000000
#>   ---                                        
#> 1004: 0.8964275 0.8874258 0.8963465 0.9062012
#> 1005: 0.9273869 0.9155699 0.9275322 0.9393796
#> 1006: 0.8391046 0.8218566 0.8399715 0.8541015
#> 1007: 0.8908591 0.8748235 0.8910117 0.9040325
#> 1008: 0.9545076 0.9428828 0.9560450 0.9665148
```
