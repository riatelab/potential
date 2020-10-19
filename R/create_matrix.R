#' @title Create a Distance Matrix Between Two Spatial Objects
#' @name create_matrix
#' @description This function creates a distance matrix between two
#' spatial objects.
#' @param x an sf object (POINT), rows of the distance matrix, row names are used as 
#' row names of the matrix.
#' @param y an sf object (POINT), columns of the distance matrix, row names are used
#' as column names of the matrix.
#' @param checksize if FALSE, bypass the distance matrix size control 
#' (see Details).
#' @param longlat if FALSE, the Euclidean distance is used, if TRUE Great Circle
#' (WGS84 ellipsoid) distance is used.
#' @details The function returns a full matrix of distances in meters.
#' If the matrix to compute is too large (more than 100,000,000 cells, more than
#' 10,000,000 origins or more than 10,000,000 destinations)
#' the function may sends a message to warn users about the amount of
#' RAM mobilized.
#' @return A distance matrix, row names are \code{x} row names, column
#' names are \code{y} row names.
#' @examples
#' g <- create_grid(x = n3_poly, res = 200000)
#' mat <- create_matrix(x = n3_pt, y = g)
#' mat[1:5, 1:5]
#' @importFrom sf st_centroid st_geometry st_geometry<- st_as_sf st_is_longlat
#' st_distance st_transform st_is
#' @importFrom methods is
#' @export
create_matrix <- function(x, y, checksize = TRUE, longlat = TRUE) {
  if (checksize) {
    nk <- nrow(x)
    nu <- nrow(y)
    if (nk * nu > 100000000 | nu > 10000000 | nk > 10000000) {
      stop(paste0("Computation aborted. The distance matrix would probably ",
      "be too large. Use checksize = FALSE to bypass this control."),
           call. = FALSE
      ) 
    }  
  }
  
  if (!st_is_longlat(x)) {
    if (longlat) {
      x <- st_transform(x, 4326)
      y <- st_transform(y, 4326)
    }
  }
  d <- st_distance(x, y)
  mat <- as.vector(d)
  dim(mat) <- dim(d)
  dimnames(mat) <- list(row.names(x), row.names(y))
  return(mat)
}
