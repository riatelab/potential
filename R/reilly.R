#' @title Reilly Catchment Areas
#' @name reilly
#' @description This function computes the catchment areas as defined by W.J. Reilly (1931).
#' @param knownpts sp or sf object; 
#' this is the set of known observations to estimate the catchment areas from.
#' @param unknownpts sp or sf object; 
#' this is the set of unknown units for which the function computes the estimates. 
#' Not used when \code{resolution} is set up. (optional)
#' @param matdist matrix; distance matrix between known observations and unknown 
#' units for which the function computes the estimates. Row names match the row 
#' names of \code{knownpts} and column names match the row names of 
#' \code{unknownpts}. \code{matdist} can contain any distance metric (time 
#' distance or euclidean distance for example). If \code{matdist} is not set, 
#' the distance matrix is built with \code{\link{CreateDistMatrix}}. (optional)
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
#' If resolution is not set, the grid will contain around 7250 points. (optional)
#' @param mask sp or sf object; the spatial extent of this object is used to 
#' create the regularly spaced points output. (optional)
#' @param bypassctrl logical; bypass the distance matrix size control (see 
#' \code{\link{CreateDistMatrix}} Details).
#' @param longlat	logical; if FALSE, Euclidean distance, if TRUE Great Circle 
#' (WGS84 ellipsoid) distance.
#' @return Point object with the computed catchment areas in a new 
#' field named \code{OUTPUT}. Values match the row names of \code{knownpts}.
#' @examples 
#' # Create a grid of paris extent and 200 meters
#' # resolution
#' data(hospital)
#' mygrid <- CreateGrid(w = hospital, resolution = 200)
#' # Create a distance matrix between known points (hospital) and mygrid
#' mymat <- CreateDistMatrix(knownpts = hospital, unknownpts = mygrid)
#' # Compute Reilly catchment areas from known points (hospital) on a given
#' # grid (mygrid) using a given distance matrix (mymat)
#' myreilly2 <- reilly(knownpts = hospital, unknownpts = mygrid,
#'                     matdist = mymat, varname = "capacity",
#'                     typefct = "exponential", span = 1250,
#'                     beta = 3, mask = paris)
#' # Compute Reilly catchment areas from known points (hospital) on a
#' # grid defined by its resolution
#' myreilly <- reilly(knownpts = hospital, varname = "capacity",
#'                    typefct = "exponential", span = 1250, beta = 3,
#'                    resolution = 200, mask = paris)
#' # The function output an sf object
#' class(myreilly)
#' # The OUTPUT field values match knownpts row names
#' head(unique(myreilly$OUTPUT))
#' @references REILLY, W. J. (1931) The law of retail gravitation, W. J. Reilly, New York.
#' @export
reilly <- function(knownpts, unknownpts, matdist, varname,
                   typefct = "exponential", span, beta, resolution, mask,
                   bypassctrl = FALSE, longlat = TRUE){
  res <- prepdata(knownpts = knownpts, unknownpts = unknownpts, 
                  matdist = matdist, bypassctrl = bypassctrl, longlat = longlat,
                  mask = mask, resolution = resolution) 
  matdens <- ComputeInteractDensity(matdist = res$matdist, typefct = typefct,
                                    beta = beta, span = span)
  matopport <- ComputeOpportunity(knownpts = res$knownpts, matdens = matdens, 
                                  varname = varname)
  unknownpts <- ComputeReilly(unknownpts = res$unknownpts, 
                              matopport = matopport)
  return(unknownpts)
}

#' @title Create a stars Raster from a Reilly Regular Grid
#' @name caReilly
#' @description This function creates a stars raster from a regularly spaced 
#' Reilly grid (output of the \code{\link{reilly}} function). 
#' @param x sf object; output of the \code{reilly} function.
#' @param mask sf object; this object is used to clip 
#' the raster. (optional)
#' @param xcoords character; name of the X coordinates field in x.
#' @param ycoords character; name of the Y coordinates field in x.
#' @param var character; name of the OUTPUT field in x.
#' @return Catchement areas
#' @examples
 #' data(hospital)
#' # Compute Reilly catchment areas from known points (hospital) on a
#' # grid defined by its resolution
#' myreilly <- reilly(knownpts = hospital, varname = "capacity",
#'                    typefct = "exponential", span = 1250, beta = 3,
#'                    resolution = 200, mask = paris)
#' @importFrom sf st_geometry st_as_sf st_bbox
#' @importFrom stars st_rasterize
#' @export
caReilly <- function(x, mask,       
                     xcoords = "COORDX",
                     ycoords = "COORDY",
                     var = "OUTPUT"){
  if(!missing(mask)){
    x <- x[st_geometry(mask),]
  }
  
  xc <- sort(unique(x[[xcoords]]))
  lx <- length(xc)
  dx <- xc[2] - xc[1]
  
  yc <- sort(unique(x[[ycoords]]))
  ly <- length(yc)
  dy <- yc[2] - yc[1]
  
  
  lv <- unique(x[[var]])
  dfv <- data.frame(idv = lv, idi = 1:length(lv))
  
  
  
  x <- merge(x, dfv, by.x = var,  by.y = "idv")
  
  
  sta <- st_rasterize(x["idi"], 
                      dx = dx, dy = dy,
                      xlim = st_bbox(x)[c(1,3)] + c(-dx, dx) / 2,
                      ylim = st_bbox(x)[c(2,4)] + c(-dy, dy) / 2)
  
  z <- st_as_sf(sta["idi"], merge = T, as_points = F, use_integer = T)
  z$idi <- dfv[match(z$idi, dfv$idi), "idv"]
  return(z)
}
