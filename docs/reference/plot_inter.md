# Display a Spatial Interaction Function

Display a spatial interaction function.

## Usage

``` r
plot_inter(fun = "e", span, beta, limit = span * 5)
```

## Arguments

- fun:

  spatial interaction function. Options are "p" (pareto, power law) or
  "e" (exponential). For pareto the interaction is defined as: (1 +
  alpha \* mDistance) ^ (-beta). For "exponential" the interaction is
  defined as: exp(- alpha \* mDistance ^ beta). The alpha parameter is
  computed from parameters given by the user (`beta` and `span`).

- span:

  distance where the density of probability of the spatial interaction
  function equals 0.5.

- beta:

  impedance factor for the spatial interaction function.

- limit:

  maximum distance used to retrieved `x` points, in map units.

## Value

a plot

## Examples

``` r
plot_inter(fun = "e", span = 2000, beta = 2, limit = 4000)

plot_inter(fun = "p", span = 2000, beta = 2, limit = 20000)
```
