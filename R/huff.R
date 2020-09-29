#' @title Huff Catchment Areas
#' @name huff
#' @description This function computes the catchment areas as defined by D. Huff (1964).
#' @param knownpts sf object; 
#' this is the set of known observations to estimate the catchment areas from.
#' @param unknownpts sf object; 
#' this is the set of unknown units for which the function computes the estimates. 
#' Not used when \code{resolution} is set up. (optional)
#' @param matdist matrix; distance matrix between known observations and unknown 
#' units for which the function computes the estimates. Row names match the row 
#' names of \code{knownpts} and column names match the row names of 
#' \code{unknownpts}. \code{matdist} can contain any distance metric (time 
#' distance or euclidean distance for example). If \code{matdist} is not set, the distance 
#' matrix is automaticly built with \code{\link{CreateDistMatrix}}. (optional)
#' @param varname character; name of the variable in the \code{knownpts} dataframe 
#' from which values are computed. Quantitative variable with no negative values. 
#' @param typefct character; spatial interaction function. Options are "pareto" 
#' (means power law) or "exponential".
#' If "pareto" the interaction is defined as: (1 + alpha * mDistance) ^ (-beta).
#' If "exponential" the interaction is defined as: 
#' exp(- alpha * mDistance ^ beta).
#' The alpha parameter is computed from parameters given by the user 
#' (\code{beta} and \code{span}).
#' @param span numeric; distance where the density of probability of the spatial 
#' interaction function equals 0.5.
#' @param beta numeric; impedance factor for the spatial interaction function.  
#' @param resolution numeric; resolution of the output grid (in map units). 
#' @param mask sf object; the spatial extent of this object is used to 
#' create the regularly spaced points output. (optional)
#' @param bypassctrl logical; bypass the distance matrix size control (see 
#' \code{\link{CreateDistMatrix}} Details).
#' @param longlat	logical; if FALSE, Euclidean distance, if TRUE Great Circle 
#' (WGS84 ellipsoid) distance.
#' @return Point object with the computed catchment areas in a new 
#' field named \code{OUTPUT}.
#' @seealso \link{starsHuff}.
#' @examples
#' library(stars)
#' library(sf)
#' # Create a grid of paris extent and 200 meters
#' # resolution
#' data(hospital)
#' mygrid <- CreateGrid(w = paris, resolution = 200)
#' # Create a distance matrix between known points (hospital) and mygrid
#' mymat <- CreateDistMatrix(knownpts = hospital, unknownpts = mygrid)
#' # Compute Huff catchment areas from known points (hospital) on a given
#' # grid (mygrid) using a given distance matrix (mymat)
#' myhuff <- huff(knownpts = hospital, unknownpts = mygrid,
#'                matdist = mymat, varname = "capacity",
#'                typefct = "exponential", span = 1250,
#'                beta = 3, mask = paris)
#' # Compute Huff catchment areas from known points (hospital) on a
#' # grid defined by its resolution
#' myhuff2 <- huff(knownpts = hospital, varname = "capacity",
#'                 typefct = "exponential", span = 1250, beta = 3,
#'                 resolution = 200, mask = paris)
#' # The two methods have the same result
#' identical(myhuff, myhuff2)
#' # the function output an sf object
#' class(myhuff)
#' @references HUFF D. (1964) Defining and Estimating a Trading Area. Journal of Marketing, 28: 34-38.
#' @importFrom methods is as
#' @importFrom sf st_as_sf
#' @export
huff <- function(knownpts, unknownpts, matdist, varname,
                 typefct = "exponential", span, beta, resolution, mask, 
                 bypassctrl = FALSE, longlat = TRUE){
  res <- prepdata(knownpts = knownpts, unknownpts = unknownpts, 
                  matdist = matdist, bypassctrl = bypassctrl, longlat = longlat,
                  mask = mask, resolution = resolution) 
  matdens <- ComputeInteractDensity(matdist = res$matdist, typefct = typefct,
                                    beta = beta, span = span)
  matopport <- ComputeOpportunity(knownpts = res$knownpts, matdens = matdens, 
                                  varname = varname)
  unknownpts <- ComputeHuff(unknownpts = res$unknownpts, matopport = matopport)
  return(unknownpts)
}

#' @title Create a Stars Raster from a Huff sf Object 
#' @name starsHuff
#' @description This function creates a stars raster from a regularly spaced 
#' Huff grid (output of the \code{\link{huff}} function). 
#' @param x sf object; output of the \code{huff} function.
#' @param mask sf POLYGON data.frame; mask used to clip 
#' the stars raster. (optional)
#' @param xcoords character; name of the X coordinates field in x.
#' @param ycoords character; name of the Y coordinates field in x.
#' @param var character; name of the OUTPUT field in x.
#' @return Stars raster of catchment areas values.
#' @seealso \link{huff}
#' @examples
#' library(stars)
#' library(sf)
#' data(hospital)
#' # Compute Huff catchment areas from known points (hospital) on a
#' # grid defined by its resolution
#' myhuff <- huff(knownpts = hospital, varname = "capacity",
#'                typefct = "exponential", span = 750, beta = 2,
#'                resolution = 100, mask = paris)
#' # Create a raster of huff values
#' myhuffstars <- starsHuff(x = myhuff, mask = paris)
#' plot(myhuffstars, nbreaks = 11, breaks = "equal", 
#'   col = hcl.colors(10, "BrBG"), 
#'   main = "Huff Catchment Areas", reset = FALSE)
#' plot(st_geometry(paris), lwd = 2, add= TRUE)
#' @importFrom sf st_bbox st_geometry
#' @importFrom stars st_rasterize 
#' @export
starsHuff <- function(x, mask,
                      xcoords = "COORDX",
                      ycoords = "COORDY",
                      var = "OUTPUT"){
  
  xc <- sort(unique(x[[xcoords]]))
  lx <- length(xc)
  dx <- xc[2] - xc[1]
  
  yc <- sort(unique(x[[ycoords]]))
  ly <- length(yc)
  dy <- yc[2] - yc[1]
  
  sta <- st_rasterize(x[var], 
                      dx = dx, dy = dy,
                      xlim = st_bbox(x)[c(1,3)] + c(-dx, dx) / 2,
                      ylim = st_bbox(x)[c(2,4)] + c(-dy, dy) / 2)
  if(!missing(mask)){
    sta <- sta[st_geometry(mask),]
  }
  
  return(sta)  
}

