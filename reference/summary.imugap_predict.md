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

  an object of class `imugap_predict` returned by `[predict()]`.

- probs:

  numeric vector of probabilities/quantiles to compute (default:
  `c(0.025, 0.5, 0.975)`).

- ...:

  additional arguments (currently ignored).

## Value

a `[data.table()]`, containing target population parameters, posterior
mean coverage (`mean`), and the requested quantiles (e.g. `q2.5`, `q50`,
`q97.5`).

## Examples

``` r
# Load example prediction object
data("predict_sim", package = "imuGAP")

# Summarize coverage predictions
summary(predict_sim)
#>       obs_c_id                  loc_id   age cohort  dose weight loc_c_id
#>          <int>                  <char> <int>  <num> <num>  <num>    <int>
#>    1:        1                   State     1     29     1      1        1
#>    2:        2                 Scruggs     1     29     1      1        2
#>    3:        3                  Simone     1     29     1      1        3
#>    4:        4                  Watson     1     29     1      1        4
#>    5:        5    Chickadee Elementary     1     29     1      1        8
#>   ---                                                                    
#> 1004:     1004     Mockingbird Academy    18     12     2      1       27
#> 1005:     1005 Kinglet Learning Center    18     12     2      1       25
#> 1006:     1006            Vireo School    18     12     2      1       28
#> 1007:     1007      Kingfisher Academy    18     12     2      1       24
#> 1008:     1008    Cormorant Elementary    18     12     2      1       22
#>            mean      q2_5       q50     q97_5
#>           <num>     <num>     <num>     <num>
#>    1: 0.0000000 0.0000000 0.0000000 0.0000000
#>    2: 0.0000000 0.0000000 0.0000000 0.0000000
#>    3: 0.0000000 0.0000000 0.0000000 0.0000000
#>    4: 0.0000000 0.0000000 0.0000000 0.0000000
#>    5: 0.0000000 0.0000000 0.0000000 0.0000000
#>   ---                                        
#> 1004: 0.9064158 0.8887300 0.9061241 0.9235849
#> 1005: 0.9839043 0.9715818 0.9848205 0.9942661
#> 1006: 0.9868587 0.9760603 0.9867051 0.9972079
#> 1007: 0.8923644 0.8793035 0.8932690 0.9030909
#> 1008: 0.9313183 0.9233198 0.9311110 0.9402193

# Summarize with custom quantiles
summary(predict_sim, probs = c(0.1, 0.5, 0.9))
#>       obs_c_id                  loc_id   age cohort  dose weight loc_c_id
#>          <int>                  <char> <int>  <num> <num>  <num>    <int>
#>    1:        1                   State     1     29     1      1        1
#>    2:        2                 Scruggs     1     29     1      1        2
#>    3:        3                  Simone     1     29     1      1        3
#>    4:        4                  Watson     1     29     1      1        4
#>    5:        5    Chickadee Elementary     1     29     1      1        8
#>   ---                                                                    
#> 1004:     1004     Mockingbird Academy    18     12     2      1       27
#> 1005:     1005 Kinglet Learning Center    18     12     2      1       25
#> 1006:     1006            Vireo School    18     12     2      1       28
#> 1007:     1007      Kingfisher Academy    18     12     2      1       24
#> 1008:     1008    Cormorant Elementary    18     12     2      1       22
#>            mean       q10       q50       q90
#>           <num>     <num>     <num>     <num>
#>    1: 0.0000000 0.0000000 0.0000000 0.0000000
#>    2: 0.0000000 0.0000000 0.0000000 0.0000000
#>    3: 0.0000000 0.0000000 0.0000000 0.0000000
#>    4: 0.0000000 0.0000000 0.0000000 0.0000000
#>    5: 0.0000000 0.0000000 0.0000000 0.0000000
#>   ---                                        
#> 1004: 0.9064158 0.8944766 0.9061241 0.9186439
#> 1005: 0.9839043 0.9764832 0.9848205 0.9908731
#> 1006: 0.9868587 0.9793914 0.9867051 0.9945030
#> 1007: 0.8923644 0.8821537 0.8932690 0.9014441
#> 1008: 0.9313183 0.9249661 0.9311110 0.9371958
```
