#' @title Compute Potentials
#' @name potential
#' @description This function computes the potentials as defined by J.Q. Stewart (1941).
#' @param x sf object; this is the set of known observations to 
#' estimate the potentials from.
#' @param y sf object; this is the set of unknown units for which 
#' the function computes the estimates. Not used when \code{res} is set 
#' up. (optional)
#' @param d matrix; distance matrix between known observations and unknown 
#' units for which the function computes the estimates. Row names match the row 
#' names of \code{x} and column names match the row names of 
#' \code{y}. \code{d} can contain any distance metric (time 
#' distance or euclidean distance for example). If \code{matdist} is missing, the distance 
#' matrix is built with \code{\link{create_matrix}}. (optional)
#' @param var character; name of the variable in the \code{x} dataframe 
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
#' \code{\link{create_matrix}} Details).
#' @return Point object with the computed potentials in a new field 
#' named \code{OUTPUT}. 
#' @examples
#' # Create a grid of paris extent and 200 meters resolution
#' data(hospital)
#' g <- create_grid(x = paris, res = 200)
#' 
#' # Create a distance matrix between known points (hospital) and points 
#' # of the grid
#' d <- create_matrix(x = hospital, y = g)
#' 
#' # Compute Stewart potentials from known points (hospital) on a given
#' # grid (g) using a given distance matrix (d)
#' pot <- potential(x = hospital, y = g,
#'                  d = d, var = "capacity",
#'                  fun = "e", span = 1000,
#'                  beta = 3, mask = paris)
#' 
#' # Compute Stewart potentials from known points (hospital) on a
#' # grid defined by its resolution
#' pot2 <- potential(x = hospital, var = "capacity",
#'                   fun = "e", span = 1250, beta = 3,
#'                   res = 200, mask = paris)
#' 
#' # The two methods have the same result
#' identical(pot, pot2)
#' # the function output a sf data.frame
#' class(pot)
#' # Computed values
#' summary(pot$OUTPUT)
#' @references 
#' STEWART J.Q. (1942) "Measure of the influence of a population at a distance", Sociometry, 5(1): 63-71.  
#' @importFrom methods is as
#' @importFrom sf st_as_sf
#' @export
potential <- function(x, y, d, var, fun = "e", span, beta, res, mask, 
                      bypassctrl = FALSE, longlat = TRUE){
  result <- prepare_data(x = x, y = y, d = d, bypassctrl = bypassctrl, 
                  longlat = longlat, mask = mask, res = res) 
  matdens <- interact_density(d = result$d, fun = fun, beta = beta, span = span)
  matopport <- opportunity(x = result$x, matdens = matdens, var = var)
  y <- compute_potentials(y = result$y, matopport = matopport)
  return(y)
}


# Internal functions
prepare_data <- function(x, y, d, bypassctrl, longlat, mask, res){
  if (!missing(y)){  
    if (!missing(d)){
      d <- use_matrix(d = d, x = x, y =  y) 
    }else{
      d <- create_matrix(x = x, y = y, bypassctrl = bypassctrl, longlat = longlat)
    }
  }else{
    if(missing(mask)){
      mask <- x
    } 
    y <- create_grid(x = mask, res = res) 
    d <- create_matrix(x = x, y = y, bypassctrl = bypassctrl, longlat = longlat) 
  }
  return(list(x = x, y = y, d = d))
}


use_matrix <- function(d, x, y){
  i <- factor(row.names(x), levels = row.names(x))
  j <- factor(row.names(y), levels = row.names(y))
  d <- d[levels(i), levels(j)]
  return(round(d, digits = 8))
}

interact_density <- function(d, fun, beta, span){
  if(fun == "p") {
    alpha  <- (2 ^ (1 / beta) - 1) / span
    matDens <- (1 + alpha * d) ^ (-beta)
  } else if(fun == "e") {
    alpha  <- log(2) / span ^ beta
    matDens <- exp(- alpha * d ^ beta)
  } else {
    stop("Please choose a valid interaction function argument (fun=)")
  }
  matDens <- round(matDens, digits = 8)
  return(matDens)
}

opportunity <- function(x, matdens, var){
  matOpport <- x[[var]] * matdens
  return(round(matOpport, digits = 8))
}

compute_potentials <- function(y, matopport){
  y$OUTPUT <- apply(matopport, 2, sum, na.rm = TRUE)
  return(y)
}

