#' @title Create a Regularly Spaced Points Grid
#' @name create_grid
#' @description This function creates a regular grid of points 
#' from the extent of a given spatial object and a given resolution.
#' @param x sf object; the spatial extent of this object is used to 
#' create the regular grid.
#' @param res numeric; resolution of the grid (in map units). If 
#' resolution is not set, the grid will contain around 5000 points. (optional)
#' @return The output of the function is an sf object of  regularly spaced 
#' points with the extent of \code{w}.
#' @seealso \link{create_matrix}
#' @examples
#' # Create a grid of paris extent and 200 meters
#' # resolution
#' library(sf)
#' data(hospital)
#' mygrid <- create_grid(x = paris, res = 200)
#' plot(st_geometry(mygrid), cex = 0.1, pch = ".")
#' plot(st_geometry(paris), border="red", lwd = 2, add = TRUE)
#' @importFrom sf st_as_sf st_crs st_bbox
#' @importFrom methods is
#' @export
create_grid <- function (x, res)
{
  bb <- st_bbox(x)
  if(missing(res)){
    k <- 5000
    s <- (bb[3] - bb[1]) / (bb[4] - bb[2])
    ny <- sqrt(k/s)
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
  g <- data.frame(ID = 1:nrow(g),
                  COORDX = g[, 1], 
                  COORDY = g[, 2])
  g <- st_as_sf(g, coords = c("COORDX", "COORDY"),
                crs = st_crs(x), remove = FALSE)
  
  return(g)
}


#' @title Create a Distance Matrix Between Two Spatial Objects
#' @name create_matrix
#' @description This function creates a distance matrix between two 
#' spatial objects.
#' @param x sf object; rows of the distance matrix.
#' @param y sf object; columns of the distance matrix.
#' @param bypassctrl logical; bypass the distance matrix size control (see Details).
#' @param longlat	logical; if FALSE, Euclidean distance, if TRUE Great Circle 
#' (WGS84 ellipsoid) distance.
#' @details The function returns a full matrix of distances in meters. 
#' If the matrix to compute is too large (more than 100,000,000 cells, more than 
#' 10,000,000 origins or more than 10,000,000 destinations) 
#' the function sends a confirmation message to warn users about the amount of 
#' RAM mobilized. 
#' Use \code{bypassctrl} = TRUE to skip this control.
#' @return A distance matrix, row names are \code{x} row names, column 
#' names are \code{y} row names.
#' @examples
#' # Create a grid of paris extent and 200 meters
#' # resolution
#' data(hospital)
#' mygrid <- create_grid(x = paris, res = 200)
#' # Create a distance matrix between known hospital and mygrid
#' mymat <- create_matrix(x = hospital, y = mygrid)
#' mymat[1:5,1:5]
#' @importFrom sf st_centroid st_geometry st_geometry<- st_as_sf st_is_longlat 
#' st_distance st_transform st_is
#' @importFrom methods is
#' @export
create_matrix  <- function(x, y, bypassctrl = FALSE, longlat = TRUE)
{
  if (bypassctrl == FALSE){
    nk <- nrow(x)
    nu <- nrow(y)
    if(nk * nu > 100000000 | nu > 10000000 | nk > 10000000){
      if (interactive()){
        cat("Do you really want to this distance matrix (from", nk , 
            "known points to", nu,"estimated values) ? \n 
            (It seems to be a heavy computation.) [y/n]" )
        z <- readLines(con = stdin(), n = 1) 
        while (!z %in% c("n","y")){
          cat ("Enter y or n")
          z <- readLines(con = stdin(), n = 1)  
        }
        if (z == "y"){
          cat("Ok, YOLO!")
        } else {
          stop("Computation aborted. Matrix would probably be too large.",
               call. = F)
        }
      } else {
        stop("Computation aborted. Matrix would probably be too large.", 
             call. = F)
      }
    }
  }
  # polygon mngmnt
  if(!is(st_geometry(x), "sfc_POINT")){
    st_geometry(x) <- st_centroid(st_geometry(x), 
                                  of_largest_polygon = all(
                                    st_is(x, "MULTIPOLYGON")))
  }
  if(!is(st_geometry(y), "sfc_POINT")){
    st_geometry(y) <- st_centroid(st_geometry(y), 
                                  of_largest_polygon = all(
                                    st_is(y, "MULTIPOLYGON")))
  }
  
  if(!st_is_longlat(x)){
    if(longlat){
      x <- st_transform(x, 4326)
      y <- st_transform(y, 4326)
    }
  }
  d <- st_distance(x, y)
  mat = as.vector(d)
  dim(mat) = dim(d)
  dimnames(mat) <- list(row.names(x), row.names(y))
  return(round(mat, digits = 2))
}
