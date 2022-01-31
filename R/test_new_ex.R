library(eurostat)
gdp_raw <- get_eurostat('nama_10r_3gdp')
pop_raw <- get_eurostat('nama_10r_3popgdp')

pop <- pop_raw[nchar(pop_raw$geo)==5 & 
                 pop_raw$time == "2018-01-01", ]
names(pop)[4] <- "pop"
pop$pop <- pop$pop * 1000

gdp <- gdp_raw[nchar(gdp_raw$geo)==5 & 
                 gdp_raw$time == "2018-01-01" &
                 gdp_raw$unit == "MIO_EUR", ]
names(gdp)[4] <- "gdp"
gdp$gdp <- gdp$gdp * 1000000
  
  
library(giscoR)
countries <- gisco_get_countries()
nuts_raw <- gisco_nuts
nuts <- nuts_raw[nuts_raw$LEVL_CODE == 3 & 
                   nuts_raw$CNTR_CODE == "IT", ]
nuts <- st_transform(nuts, 3035)
countries <- st_transform(countries, 3035)


nuts <- merge(nuts, pop[,c("geo", "pop")], by.x = "NUTS_ID", by.y = "geo", all.x = T)
nuts <- merge(nuts, gdp[,c("geo", "gdp")], by.x = "NUTS_ID", by.y = "geo", all.x = T)


nuts$gdp_hab <- nuts$gdp / nuts$pop
bv <- quantile(nuts$gdp_hab, seq(from = 0, to = 1, length.out = 9))


par(mfrow = c(1,3))
pal <- mf_get_pal(n = 9, palette = "Burg", rev = TRUE)
mf_init(nuts)
mf_map(countries, add = T, col = "grey90", border = "grey80")
mf_map(nuts, "gdp_hab", "choro", breaks = bv, add = T, pal = pal, border = NA)



nuts_pt <- nuts
st_geometry(nuts_pt) <- st_centroid(st_geometry(nuts_pt))
d <- create_matrix(nuts_pt, nuts_pt)

# Compute the potentials of population and GDP per units
# function = exponential, beta = 2, span = 75 km
pot <- potential(x = nuts_pt, 
                 y = nuts_pt, 
                 d = d, 
                 var = c("pop", "gdp"), 
                 fun = "e",  
                 beta = 2, 
                 span = 100000)
# A the potential GDP per capita
nuts$gdpcappot <-  pot[, 2]  / pot[, 1]
# Discretize the variable
bv2 <- c(min(nuts$gdpcappot), bv[2:8], max(nuts$gdpcappot))

mf_init(nuts)
mf_map(countries, add = T, col = "grey90", border = "grey80")
mf_map(nuts, "gdpcappot", "choro", breaks = bv2, add = T, pal = pal, border = NA)




# Compute the potentials of population on a regular grid (50km span)
g <- create_grid(x = nuts, res = 10000)
d <- create_matrix(nuts_pt, g)
# function = exponential, beta = 2, span = 75 km
pot2 <- potential(x = nuts_pt, 
                  y = g, 
                  d = d, 
                  var = c("pop", "gdp"), 
                  fun = "e",  
                  beta = 2, 
                  span = 100000)
# Create the ratio variable
g$gdpcappot <-  pot2[, 2] / pot2[, 1]

# Create an isopleth layer
pot <- equipotential(x = g, var = "gdpcappot", breaks = bv, mask = nuts)
# Get breaks values
bv3 <- sort(c(unique(pot$min), max(pot$max)), decreasing = FALSE)
# Draw the map
mf_init(nuts)
mf_map(countries, add = T, col = "grey90", border = "grey80")
mf_map(pot, "center", "choro", breaks = bv3, add = T, border = NA, pal = pal)
mf_scale()

