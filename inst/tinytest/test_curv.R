suppressPackageStartupMessages(library(sf))


# test plot_inter
expect_silent(plot_inter(fun="e", span = 2000, beta = 2))
expect_silent(plot_inter(fun="p", span = 2000, beta = 2))

# test create_grid
expect_silent(create_grid(x = n3_poly, res = 300000))

# test create_matrix
g <- create_grid(x = n3_poly, res = 300000)
expect_silent(create_matrix(x = g, y = g, checksize = TRUE, 
                            longlat = FALSE))

if (sf::sf_extSoftVersion()[["GDAL"]] >= "3.0.4") {
  expect_silent(create_matrix(x = g, y = g, longlat = TRUE))
}
expect_silent(create_matrix(x = g, y = g, checksize = FALSE, 
                            longlat = TRUE))
huge <- do.call(rbind, 
                list(n3_pt, n3_pt, n3_pt, n3_pt, n3_pt, 
                     n3_pt, n3_pt, n3_pt, n3_pt, 
                     n3_pt, n3_pt, n3_pt, n3_pt, 
                     n3_pt, n3_pt, n3_pt))
expect_error(create_matrix(x = huge, 
                           y = huge, 
                           checksize = TRUE, 
                           longlat = FALSE))
# test potential
d <- create_matrix(n3_pt, g)
expect_silent(potential(x = n3_pt, y = g, d = d, var = "POP19", 
                        fun = "e", span = 100000, beta = 2))

expect_silent(potential(x = n3_pt, y = g, d = d, var = "POP19", 
                        fun = "p", span = 100000, beta = 2))

# test equipotential
pot <- potential(
  x = n3_pt, y = g, d = d, var = "POP19",
  fun = "e", span = 200000, beta = 2)
g$OUTPUT <- pot

expect_silent(equipotential(g, var = "OUTPUT", mask = n3_poly))
expect_silent(equipotential(g, var = "OUTPUT", mask = n3_poly))
expect_silent(equipotential(g, var = "OUTPUT"))
expect_silent(equipotential(g, var = "OUTPUT", 
                            breaks = c(0,1000,1000000,40000000)))
expect_silent(equipotential(g[,-c(2,3)], var = "OUTPUT", mask = n3_poly))
expect_silent(equipotential(g[,-c(2,3)], var = "OUTPUT", 
                            mask = n3_poly, buffer = 100000))


# test mcpotential
home <- length(unclass(packageVersion("potential"))[[1]]) == 4
if(home){
  expect_silent(mcpotential(
    x = n3_pt, y = g, var = "POP19",
    fun = "e", span = 200000, beta = 2))
  expect_silent(mcpotential(
    x = n3_pt, y = g, var = c("POP19", "POP18"),
    fun = "e", span = 200000, beta = 2))
  expect_silent(mcpotential(
    x = n3_pt, y = g, var = "POP19",
    fun = "p", span = 200000, beta = 2, ncl = 1))
}



expect_error(create_grid(x = 3, res = 100))

expect_error(potential:::test_point(3, "E"))

expect_silent(potential:::test_point(g, "E"))

