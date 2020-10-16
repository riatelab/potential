suppressPackageStartupMessages(library(sf))
library(sf)
expect_silent(plot_inter(fun="e", span = 2000, beta = 2))
expect_silent(plot_inter(fun="p", span = 2000, beta = 2))

expect_silent(create_grid(x = n3_poly, res = 300000))

g <- create_grid(x = n3_poly, res = 300000)
expect_silent(create_matrix(x = g, y = g, checksize = T, longlat = F))
expect_silent(create_matrix(x = g, y = g, checksize = F, longlat = T))

huge <- do.call(rbind, 
                list(n3_pt, n3_pt, n3_pt, n3_pt, n3_pt, 
                     n3_pt, n3_pt, n3_pt, n3_pt, 
                     n3_pt, n3_pt, n3_pt, n3_pt, 
                     n3_pt, n3_pt, n3_pt))
expect_error(create_matrix(x = huge, 
                            y = huge, 
                            checksize = T, longlat = T))

d <-  create_matrix(n3_pt, g)
expect_silent(potential(x = n3_pt, y = g, d = d, var = "POP19", 
                        fun = "e", span = 100000, beta = 2))

expect_silent(potential(x = n3_pt, y = g, d = d, var = "POP19", 
                        fun = "p", span = 100000, beta = 2))


pot <- potential(
  x = n3_pt, y = g, d = d, var = "POP19",
  fun = "e", span = 200000, beta = 2)
g$OUTPUT <- pot
expect_silent(equipotential(g, var = "OUTPUT", mask = n3_poly))
expect_silent(equipotential(g, var = "OUTPUT"))
expect_silent(equipotential(g, var = "OUTPUT", breaks = c(0,1000,1000000,40000000)))
              


home <- length(unclass(packageVersion("potential"))[[1]]) == 4

if(home){
  expect_silent(mcpotential(
    x = n3_pt, y = g, var = "POP19",
    fun = "e", span = 200000, beta = 2))
  expect_silent(mcpotential(
    x = n3_pt, y = g, var = "POP19",
    fun = "p", span = 200000, beta = 2, ncl = 1))
}
