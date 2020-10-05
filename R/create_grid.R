#' @title Create a Regularly Spaced Points Grid
#' @name create_grid
#' @description This function creates a regular grid of points
#' from the extent of a given spatial object and a given resolution.
#' @param x an sf object, the spatial extent of this object is used to
#' create the regular grid.
#' @param res resolution of the grid (in map units). If \code{res} is not set 
#' the grid will contain around 5000 points. 
#' @return The output of the function is an sf object of regularly spaced
#' points with the extent of x.
#' @examples
#' library(sf)
#' g <- create_grid(x = n3_poly, res = 200000)
#' plot(st_geometry(g))
#' plot(st_geometry(n3_poly), border = "red", add = TRUE)
#' @importFrom sf st_as_sf st_crs st_bbox
#' @importFrom methods is
#' @export
create_grid <- function(x, res) {
  bb <- st_bbox(x)
  if (missing(res)) {
    k <- 5000
    s <- (bb[3] - bb[1]) / (bb[4] - bb[2])
    ny <- sqrt(k / s)
    nx <- s * ny
    gx <- seq(bb[1], bb[3], length.out = nx)
    gy <- seq(bb[2], bb[4], length.out = ny)
    res <- (mean(c(gx[2] - gx[1], gy[2] - gy[1])))
  }

  rounder <- bb %% res
  bb[1:2] <- bb[1:2] - rounder[1:2]
  bb[3:4] <- bb[3:4] + res - rounder[3:4]
  cx <- seq(from = bb[1], to = bb[3], by = res)
  cy <- seq(from = bb[2], to = bb[4], by = res)

  g <- expand.grid(cx, cy)
  g <- data.frame(
    ID = 1:nrow(g),
    COORDX = g[, 1],
    COORDY = g[, 2]
  )
  g <- st_as_sf(g,
    coords = c("COORDX", "COORDY"),
    crs = st_crs(x), remove = FALSE
  )

  return(g)
}