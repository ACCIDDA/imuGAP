# Check if an object is canonical

Check if an object is canonical

## Usage

``` r
is_canonical(dt, target_class)
```

## Arguments

- dt:

  a `[data.table()]` (or compatible object)

- target_class:

  a string, one of 'locations', 'observations', 'populations'

## Value

`TRUE` if `dt` is canonical for `target_class`, `FALSE` otherwise.
