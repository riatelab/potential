# Create a Distance Matrix Between Two Spatial Objects

This function creates a distance matrix between two spatial objects.

## Usage

``` r
create_matrix(x, y, checksize = TRUE, longlat = FALSE)
```

## Arguments

- x:

  an sf object (POINT), rows of the distance matrix, row names are used
  as row names of the matrix.

- y:

  an sf object (POINT), columns of the distance matrix, row names are
  used as column names of the matrix.

- checksize:

  if FALSE, bypass the distance matrix size control (see Details).

- longlat:

  if FALSE, the Euclidean distance is used, if TRUE Great Circle (WGS84
  ellipsoid) distance is used.

## Value

A distance matrix, row names are `x` row names, column names are `y` row
names.

## Details

The function returns a full matrix of distances in meters. If the matrix
to compute is too large (more than 100,000,000 cells, more than
10,000,000 origins or more than 10,000,000 destinations) the function
may sends a message to warn users about the amount of RAM mobilized.

## Examples

``` r
g <- create_grid(x = n3_poly, res = 200000)
mat <- create_matrix(x = n3_pt, y = g)
mat[1:5, 1:5]
#>         1       2       3       4       5
#> 1 2674796 2482621 2291785 2102653 1915730
#> 2 2622868 2430532 2239539 2050264 1863231
#> 3 2682813 2492081 2302922 2115758 1931170
#> 4 2643075 2451462 2261302 2072995 1887096
#> 5 2638427 2447799 2258792 2071850 1887586
```
