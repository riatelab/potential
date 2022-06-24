#' @title Create Polygons of Equipotential
#' @name equipotential
#' @description
#' This function creates polygons of equipotential from a regular grid of
#' potential points.
#' @param x an sf object of regularly spaced points.
#' @param nclass a number of class.
#' @param breaks a vector of break values.
#' @param mask an sf object of polygons or multipolygons. \code{mask} is  used
#' to clip polygons of contours equipotential.
#' @param xcoords not used.
#' @param ycoords not used.
#' @param var name of the variable to use in \code{x}.
#' @param buffer if set, a buffer is added to the mask in order to 
#' reach more precisely the number of breaks. The buffer is defined in 
#' \code{x} units.
#' @return The output is an sf object (POLYGONS). The data frame contains four
#' fields: id (id of each polygon), min and max (minimum and maximum breaks of
#' the polygon) and center (central values of classes).
#' @importFrom sf st_as_sf st_crs st_bbox st_cast st_sf st_sfc st_intersection
#' st_union st_agr<- st_collection_extract st_make_valid st_buffer st_coordinates
#' @importFrom mapiso mapiso
#' @examples
#' library(sf)
#' y <- create_grid(x = n3_poly, res = 200000)
#' d <- create_matrix(n3_pt, y)
#' pot <- potential(
#'   x = n3_pt, y = y, d = d, var = "POP19",
#'   fun = "e", span = 200000, beta = 2
#' )
#' y$OUTPUT <- pot
#' equipot <- equipotential(y, var = "OUTPUT", mask = n3_poly)
#' plot(equipot["center"], pal = hcl.colors(nrow(equipot), "cividis"))
#' @export
equipotential <- function(x,
                          var,
                          nclass = 8,
                          breaks,
                          mask,
                          buffer, 
                          xcoords,
                          ycoords) {
  
  if (!missing(buffer)){
    mask_b <- sf::st_buffer(mask, buffer)
    inter <- st_intersects(x = x, y = mask_b)
    inout <- sapply(inter, function(x)if(length(x)>0){1}else{NA})
    x[[var]] <- inout * x[[var]]
  }
  
  iso <- mapiso(x = x, var = var, 
                breaks = breaks, 
                nbreaks = nclass, 
                mask = mask)
  names(iso)[1:3] <- c("id", "min", "max")
  iso$center <- iso$min + (iso$max - iso$min) / 2
  
  return(iso)
}
