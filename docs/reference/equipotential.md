# Create Polygons of Equipotential

This function creates polygons of equipotential from a regular grid of
potential points.

## Usage

``` r
equipotential(x, var, nclass = 8, breaks, mask, buffer, xcoords, ycoords)
```

## Arguments

- x:

  an sf object of regularly spaced points.

- var:

  name of the variable to use in `x`.

- nclass:

  a number of class.

- breaks:

  a vector of break values.

- mask:

  an sf object of polygons or multipolygons. `mask` is used to clip
  polygons of contours equipotential.

- buffer:

  if set, a buffer is added to the mask in order to reach more precisely
  the number of breaks. The buffer is defined in `x` units.

- xcoords:

  not used.

- ycoords:

  not used.

## Value

The output is an sf object (POLYGONS). The data frame contains four
fields: id (id of each polygon), min and max (minimum and maximum breaks
of the polygon) and center (central values of classes).

## Examples

``` r
library(sf)
y <- create_grid(x = n3_poly, res = 200000)
d <- create_matrix(n3_pt, y)
pot <- potential(
  x = n3_pt, y = y, d = d, var = "POP19",
  fun = "e", span = 200000, beta = 2
)
y$OUTPUT <- pot
equipot <- equipotential(y, var = "OUTPUT", mask = n3_poly)
plot(equipot["center"])
```
