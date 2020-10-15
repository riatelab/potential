
# potential

[![R-CMD-check](https://github.com/riatelab/potential/workflows/R-CMD-check/badge.svg)](https://github.com/riatelab/potential/actions)

This package provides functions to compute potential models as defined
by Stewart (1941). Several options are available to customize the
models, such as the possibility to fine-tune the distance friction
functions or to use custom distance matrices. Some computations are
parallelized to improve their efficiency.

## Installation

### From GitHub

Development version / unstable

``` r
require(remotes)
remotes::install_github("riatelab/potential")
```

## Demo

``` r
library(sf)
```

    ## Linking to GEOS 3.7.1, GDAL 3.1.2, PROJ 7.1.0

``` r
library(potential)

plot(st_geometry(n3_poly), col = "lightblue")
plot(st_geometry(n3_pt), add = TRUE)
```

![](man/figures/unnamed-chunk-2-1.png)<!-- -->

``` r
g <- create_grid(x = n3_poly, res = 100000)

plot(st_geometry(n3_poly), col = "lightblue")
plot(st_geometry(n3_pt), add = TRUE)
plot(st_geometry(g), cex = .2, add = TRUE)
```

![](man/figures/unnamed-chunk-2-2.png)<!-- -->

``` r
d <- create_matrix(x = n3_pt, g)
knitr::kable(d[1:5, 1:5], row.names = T)
```

|   |       1 |       2 |       3 |       4 |       5 |
| :- | ------: | ------: | ------: | ------: | ------: |
| 1 | 2671654 | 2574653 | 2478056 | 2381893 | 2286198 |
| 2 | 2619775 | 2522716 | 2426061 | 2329840 | 2234091 |
| 3 | 2681131 | 2584765 | 2488856 | 2393440 | 2298559 |
| 4 | 2640664 | 2543918 | 2447601 | 2351746 | 2256394 |
| 5 | 2637018 | 2540710 | 2444868 | 2349532 | 2254745 |

``` r
prob_interaction(fun = "e", span = 75000, beta = 2, limit = 250000)
```

![](man/figures/unnamed-chunk-3-1.png)<!-- -->

``` r
g$pot <- potential(
  x = n3_pt, y = g,
  d = d, var = "POP19",
  fun = "e", span = 75000,
  beta = 2
)

plot(g["pot"], pch= 20, cex = 1.2)
```

![](man/figures/unnamed-chunk-4-1.png)<!-- -->

``` r
equipot <- equipotential(g, var = "pot", mask = n3_poly)
plot(st_geometry(equipot), col = hcl.colors(nrow(equipot), "cividis"))
```

![](man/figures/unnamed-chunk-5-1.png)<!-- -->

## Note

This package provides access to the revamped potential-related functions
initialy offered by
[`SpatialPosition`](https://CRAN.R-project.org/package=SpatialPosition).

## References

<div id="refs" class="references">

<div id="ref-STEWART41">

Stewart, John Q. 1941. “An Inverse Distance Variation for Certain Social
Influences.” *Science* 93 (2404): 89–90.
<https://doi.org/10.1126/science.93.2404.89>.

</div>

</div>
