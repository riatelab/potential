#' @title Compute Potentials
#' @name potential
#' @description This function computes potentials as defined 
#' by J.Q. Stewart (1941).
#' @param x an sf object of points, the set of known observations to
#' estimate the potentials from.
#' @param y an sf object of points, the set of unknown units for which
#' the function computes the estimates. 
#' @param d a distance matrix between known observations and unknown
#' units for which the function computes the estimates. Row names match the row
#' names of \code{x} and column names match the row names of
#' \code{y}. \code{d} can contain any distance metric (time
#' distance or euclidean distance for example). 
#' @param var names of the variables in \code{x} from which potentials are 
#' computed. Quantitative variables with no negative values.
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
#' @return If only one variable is computed a vector is returned, if more than
#' one variable is computed a matrix is returned.
#' @examples
#' library(sf)
#' y <- create_grid(x = n3_poly, res = 200000)
#' d <- create_matrix(n3_pt, y)
#' pot <- potential(
#'   x = n3_pt, y = y, d = d, var = "POP19",
#'   fun = "e", span = 200000, beta = 2)
#' y$OUTPUT <- pot
#' equipot <- equipotential(y, var = "OUTPUT", mask = n3_poly)
#' plot(equipot['center'], pal = hcl.colors(nrow(equipot), "cividis"))
#' @references
#' STEWART, JOHN Q. 1941. "An Inverse Distance Variation for Certain Social 
#' Influences." \emph{Science} 93 (2404): 89–90. 
#' \url{https://doi.org/10.1126/science.93.2404.89}.
#' @importFrom methods is as
#' @importFrom sf st_as_sf
#' @export
potential <- function(x, y, d, var, fun, span, beta) {
  result <- prepare_data(x = x, y = y, d = d)
  matdens <- interact_density(d = result$d, 
                              fun = fun, beta = beta, span = span)
  
  pot <- apply(X = result$x[, var, drop = FALSE], MARGIN = 2, 
              FUN = compute_potentials, matdens)
  
  if (length(var) == 1) {
    pot <- as.numeric(pot)
  }
  
  return(pot)
}


# Internal functions
#' @importFrom sf st_drop_geometry
prepare_data <- function(x, y, d) {
  d <- use_matrix(d = d, x = x, y = y)
  x <- st_drop_geometry(x)
  y <- st_drop_geometry(y)
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
  } 
  return(matDens)
}

compute_potentials <- function(x, matdens, var) {
  pot <- apply(x * matdens, 2, sum, na.rm = TRUE)
}
