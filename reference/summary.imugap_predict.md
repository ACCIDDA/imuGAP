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
#> 1004: 0.8943781 0.8741308 0.8947423 0.9109003
#> 1005: 0.9757563 0.9636538 0.9753240 0.9869096
#> 1006: 0.9784638 0.9629755 0.9786510 0.9891350
#> 1007: 0.8781338 0.8588578 0.8770492 0.8970955
#> 1008: 0.9193504 0.9048380 0.9202304 0.9306611

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
#> 1004: 0.8943781 0.8837599 0.8947423 0.9054732
#> 1005: 0.9757563 0.9679561 0.9753240 0.9833429
#> 1006: 0.9784638 0.9705036 0.9786510 0.9855303
#> 1007: 0.8781338 0.8674319 0.8770492 0.8910474
#> 1008: 0.9193504 0.9126196 0.9202304 0.9262668
```
