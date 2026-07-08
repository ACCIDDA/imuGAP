# Convert coverage predictions to a data.frame

Converts the 3D draws array of an `imugap_predict` object into a
long-format `data.frame` containing `iteration`, `chain`, target
metadata, and a `coverage` column.

## Usage

``` r
# S3 method for class 'imugap_predict'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  an `imugap_predict` object returned by `[predict()]`.

- row.names:

  `NULL` or a character vector giving the row names for the data frame.

- optional:

  logical. If `TRUE`, setting row names and converting column names is
  optional.

- ...:

  additional arguments (currently ignored).

## Value

A `data.table` with columns `iteration`, `chain`, the target metadata
columns, and `coverage`.

## Examples

``` r
# Load example prediction object
data("predict_sim", package = "imuGAP")

# Convert predictions to a data.frame/data.table
df <- as.data.frame(predict_sim)
head(df)
#>    iteration chain obs_c_id loc_id   age cohort  dose weight loc_c_id coverage
#>        <int> <int>    <int> <char> <int>  <num> <num>  <num>    <int>    <num>
#> 1:         1     1        1  State     1     30     1      1        1        0
#> 2:         2     1        1  State     1     30     1      1        1        0
#> 3:         3     1        1  State     1     30     1      1        1        0
#> 4:         4     1        1  State     1     30     1      1        1        0
#> 5:         5     1        1  State     1     30     1      1        1        0
#> 6:         6     1        1  State     1     30     1      1        1        0
```
