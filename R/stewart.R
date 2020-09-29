#' @title Stewart Potentials
#' @name stewart
#' @description This function computes the potentials as defined by J.Q. Stewart (1942).
#' @param x sf object; this is the set of known observations to 
#' estimate the potentials from.
#' @param y sf object; this is the set of unknown units for which 
#' the function computes the estimates. Not used when \code{resolution} is set 
#' up. (optional)
#' @param d matrix; distance matrix between known observations and unknown 
#' units for which the function computes the estimates. Row names match the row 
#' names of \code{knownpts} and column names match the row names of 
#' \code{unknownpts}. \code{matdist} can contain any distance metric (time 
#' distance or euclidean distance for example). If \code{matdist} is missing, the distance 
#' matrix is built with \code{\link{CreateDistMatrix}}. (optional)
#' @param var character; name of the variable in the \code{knownpts} dataframe 
#' from which potentials are computed. Quantitative variable with no negative values. 
#' @param fun spatial interaction function. Options are "p"
#' (pareto, power law) or "e" (exponential).
#' For pareto the interaction is defined as: (1 + alpha * mDistance) ^ (-beta).
#' For "exponential" the interaction is defined as:
#' exp(- alpha * mDistance ^ beta).
#' The alpha parameter is computed from parameters given by the user
#' (\code{beta} and \code{span}).
#' @param span numeric; distance where the density of probability of the spatial 
#' interaction function equals 0.5.
#' @param beta numeric; impedance factor for the spatial interaction function.  
#' @param res numeric; resolution of the output grid (in map units). 
#' If resolution is not set, the grid will contain around 7250 points. (optional)
#' @param mask sf object; the spatial extent of this object is used to 
#' create the regularly spaced points output. (optional)
#' @param longlat	logical; if FALSE, Euclidean distance, if TRUE Great Circle 
#' (WGS84 ellipsoid) distance.
#' @param bypassctrl logical; bypass the distance matrix size control (see 
#' \code{\link{CreateDistMatrix}} Details).
#' @return Point object with the computed potentials in a new field 
#' named \code{OUTPUT}. 
#' @seealso \link{isopoly}.
#' @examples
#' # Create a grid of paris extent and 200 meters
#' # resolution
#' data(hospital)
#' mygrid <- CreateGrid(w = paris, resolution = 200)
#' # Create a distance matrix between known points (hospital) and mygrid
#' mymat <- CreateDistMatrix(knownpts = hospital, unknownpts = mygrid)
#' # Compute Stewart potentials from known points (hospital) on a given
#' # grid (mygrid) using a given distance matrix (mymat)
#' mystewart <- stewart(x = hospital, y = mygrid,
#'                      d = mymat, var = "capacity",
#'                      fun = "exponential", span = 1250,
#'                      beta = 3, mask = paris)
#' # Compute Stewart potentials from known points (hospital) on a
#' # grid defined by its resolution
#' mystewart2 <- stewart(x = hospital, var = "capacity",
#'                       fun = "exponential", span = 1250, beta = 3,
#'                       res = 200, mask = paris)
#' # The two methods have the same result
#' identical(mystewart, mystewart2)
#' # the function output a sf data.frame
#' class(mystewart)
#' # Computed values
#' summary(mystewart$OUTPUT)
#' @references 
#' STEWART J.Q. (1942) "Measure of the influence of a population at a distance", Sociometry, 5(1): 63-71.  
#' @importFrom methods is as
#' @importFrom sf st_as_sf
#' @export
stewart <- function(x, y, d, var, fun = "e", span, beta, res, mask, 
                    bypassctrl = FALSE, longlat = TRUE){
  res <- prepdata(knownpts = x, unknownpts = y, 
                  matdist = d, bypassctrl = bypassctrl, longlat = longlat,
                  mask = mask, resolution = res) 
  matdens <- ComputeInteractDensity(matdist = res$matdist, typefct = fun,
                                    beta = beta, span = span)
  matopport <- ComputeOpportunity(knownpts = res$knownpts, matdens = matdens, 
                                  varname = var)
  unknownpts <- ComputePotentials(unknownpts = res$unknownpts, 
                                  matopport = matopport)
  return(unknownpts)
}



stewart_to_sf

