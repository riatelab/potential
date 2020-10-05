#' @title Compute Potentials
#' @name potential
#' @description This function computes potentials as defined 
#' by J.Q. Stewart (1941).
#' @param x an sf object, the set of known observations to
#' estimate the potentials from.
#' @param y an sf object, the set of unknown units for which
#' the function computes the estimates. Not used when \code{res} is set
#' up.
#' @param d a distance matrix between known observations and unknown
#' units for which the function computes the estimates. Row names match the row
#' names of \code{x} and column names match the row names of
#' \code{y}. \code{d} can contain any distance metric (time
#' distance or euclidean distance for example). If \code{d} is missing, 
#' the distance matrix is internaly built with \code{\link{create_matrix}}. 
#' @param var name of the variable in \code{x} from which potentials are 
#' computed. Use only quantitative variable with no negative values.
#' @param fun spatial interaction function. Options are "p"
#' (pareto, power law) or "e" (exponential).
#' For pareto the interaction is defined as: (1 + alpha * mDistance) ^ (-beta).
#' For "exponential" the interaction is defined as:
#' exp(- alpha * mDistance ^ beta).
#' The alpha parameter is computed from parameters given by the user
#' (\code{beta} and \code{span}).
#' @param span distance where the density of probability of the spatial
#' interaction function equals 0.5.
#' @param beta impedance factor for the spatial interaction function.
#' @param res resolution of the output grid (in map units).
#' If \code{res} is not set, the grid will contain around 5000 points. 
#' @param mask an sf object, the spatial extent of this object is used to
#' create the regularly spaced points output if \code{y} is not set. 
#' @param bypassctrl if TRUE, bypass the distance matrix size control 
#' (see \link{create_matrix} Details).
#' @param longlat if FALSE, the Euclidean distance is used, if TRUE Great Circle
#' (WGS84 ellipsoid) distance is used.
#' @return sf object with the computed potentials in a new field
#' named \code{OUTPUT}.
#' @examples
#' library(sf)
#' pot <- potential(
#'   x = n3_pt, var = "POP19",
#'   fun = "e", span = 200000, beta = 2,
#'   res = 200000, mask = n3_poly
#' )
#' equipot <- equipotential(pot, mask = n3_poly)
#' plot(equipot['center'], pal = hcl.colors(nrow(equipot), "cividis"))
#' @references
#' STEWART, JOHN Q. 1941. "An Inverse Distance Variation for Certain Social 
#' Influences." \emph{Science} 93 (2404): 89–90. 
#' \url{https://doi.org/10.1126/science.93.2404.89}.
#' @importFrom methods is as
#' @importFrom sf st_as_sf
#' @export
potential <- function(x, y, d, var, fun = "e", span, beta, res, mask,
                      bypassctrl = FALSE, longlat = TRUE) {
  result <- prepare_data(
    x = x, y = y, d = d, bypassctrl = bypassctrl,
    longlat = longlat, mask = mask, res = res
  )
  matdens <- interact_density(d = result$d, fun = fun, beta = beta, span = span)
  matopport <- opportunity(x = result$x, matdens = matdens, var = var)
  y <- compute_potentials(y = result$y, matopport = matopport)
  return(y)
}


# Internal functions
prepare_data <- function(x, y, d, bypassctrl, longlat, mask, res) {
  if (!missing(y)) {
    if (!missing(d)) {
      d <- use_matrix(d = d, x = x, y = y)
    } else {
      d <- create_matrix(x = x, y = y, bypassctrl = bypassctrl, 
                         longlat = longlat)
    }
  } else {
    if (missing(mask)) {
      mask <- x
    }
    y <- create_grid(x = mask, res = res)
    d <- create_matrix(x = x, y = y, bypassctrl = bypassctrl, longlat = longlat)
  }
  return(list(x = x, y = y, d = d))
}


use_matrix <- function(d, x, y) {
  i <- factor(row.names(x), levels = row.names(x))
  j <- factor(row.names(y), levels = row.names(y))
  d <- d[levels(i), levels(j)]
  return(round(d, digits = 8))
}

interact_density <- function(d, fun, beta, span) {
  if (fun == "p") {
    alpha <- (2^(1 / beta) - 1) / span
    matDens <- (1 + alpha * d)^(-beta)
  } else if (fun == "e") {
    alpha <- log(2) / span^beta
    matDens <- exp(-alpha * d^beta)
  } else {
    stop("Please choose a valid interaction function argument (fun=)")
  }
  matDens <- round(matDens, digits = 8)
  return(matDens)
}

opportunity <- function(x, matdens, var) {
  matOpport <- x[[var]] * matdens
  return(round(matOpport, digits = 8))
}

compute_potentials <- function(y, matopport) {
  y$OUTPUT <- apply(matopport, 2, sum, na.rm = TRUE)
  return(y)
}
