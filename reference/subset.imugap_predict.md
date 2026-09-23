# Subset coverage predictions

Subsets predicted coverage draws by target metadata (variables),
iterations, and chains.

## Usage

``` r
# S3 method for class 'imugap_predict'
subset(x, subset, iteration, chain, ...)
```

## Arguments

- x:

  an object of class `imugap_predict` returned by `[predict()]`.

- subset:

  logical expression indicating which target variables to keep.
  Evaluated in the context of the `target` metadata `[data.table()]`.

- iteration:

  numeric, integer, or logical vector of iterations to keep.

- chain:

  numeric, integer, or logical vector of chains to keep.

- ...:

  additional arguments (currently ignored).

## Value

an object of class `imugap_predict`, subsetted with corresponding
`draws` and `target` metadata.

## Examples

``` r
# Load example prediction object
data("predict_sim", package = "imuGAP")

# Subset predictions by target metadata
subset(predict_sim, dose == 2)
#> An imuGAP predictions object (`imugap_predict`):
#>   Targets:   504 target population slices across 28 locations
#>   Posterior: 100 draws (4 chains x 25 iterations)
#> 
#> Use summary() to compute quantiles or as.data.frame() to convert to a long table.

# Subset predictions by iteration and chain
subset(predict_sim, iteration = 1:10, chain = 1)
#> An imuGAP predictions object (`imugap_predict`):
#>   Targets:   1008 target population slices across 28 locations
#>   Posterior: 10 draws (1 chain x 10 iterations)
#> 
#> Use summary() to compute quantiles or as.data.frame() to convert to a long table.
```
