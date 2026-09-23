# Print coverage predictions

Prints a concise summary of an `imugap_predict` object, including target
grid dimensions and posterior draw dimensions.

## Usage

``` r
# S3 method for class 'imugap_predict'
print(x, ...)
```

## Arguments

- x:

  an object of class `imugap_predict` returned by `[predict()]`.

- ...:

  additional arguments (currently ignored).

## Value

invisibly returns `x`.

## Examples

``` r
data("predict_sim", package = "imuGAP")
print(predict_sim)
#> An imuGAP predictions object (`imugap_predict`):
#>   Targets:   1008 target population slices across 28 locations
#>   Posterior: 100 draws (4 chains x 25 iterations)
#> 
#> Use summary() to compute quantiles or as.data.frame() to convert to a long table.
```
