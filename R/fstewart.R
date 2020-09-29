#' @title Fast Stewart Potentials
#' @description Stewart potentials with a cutoff distance and parallel computation.
#' @param x set of points to compute the potentials from, sf points.
#' @param y set of points for which the potentials are computed, sf points.
#' @param var names of the variables in \code{x} from which potentials are computed.
#' Quantitative variables with no negative values.
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
#' @param limit maximum distance used to retrieve \code{x} points, in map units.
#' @param ncl number of clusters. \code{ncl} is set to
#' \code{parallel::detectCores() - 1} by default.
#' @param size \code{fstewart} splits \code{y} in smaller chunks and
#' dispatches the computation in \code{ncl} cores, \code{size} indicates the
#' size of each chunks.
#' @return If only one variable is computed a vector is returned, if more than
#' one variable is computed a matrix is returned.
#' @export
#' @importFrom sf st_buffer st_centroid st_geometry st_intersects
#' @examples
#' \dontrun{
#' # The dataset is in cartography package
#' }
fstewart <- function(x, y, var = "v", fun = "e",
                     span = 2000, beta = 2,
                     limit = 3 * span, ncl = 3, size = 500){
  # launch multiple cores
  if (missing(ncl)){
    ncl <- parallel::detectCores(all.tests = FALSE, logical = FALSE) - 1 
    if(is.na(ncl)){
      ncl <- 1 
    }
    if(ncl == 0){
      ncl <- 1
    }
  }
  ##
  cl <- parallel::makeCluster(ncl, setup_strategy = "sequential")
  doParallel::registerDoParallel(cl)
  
  # data simplification
  xsfc <- st_geometry(x)
  kgeom <- matrix(unlist(xsfc), ncol = 2, byrow = TRUE)
  
  v <- as.matrix(x = x[,var,drop = TRUE])
  
  ysfc <- st_geometry(y)
  
  # sequence to split unknowpts
  ny <- nrow(y)
  sequence <- unique(c(seq(1, ny, size), ny + 1))
  lseq <- length(sequence) - 1
  
  # split unknownpts and put it on a list
  ml <- list()
  for  (i in 1:lseq){
    ml[[i]] <- ysfc[(sequence[i]):(sequence[i+1]-1)]
  }
  
  # dispatch
  pot <- foreach::`%dopar%`(
    foreach::foreach(ysfc = ml,
                     .packages = 'sf',
                     .combine = c,
                     .inorder = FALSE),
    {
      # FUNS
      eucledian_simple <- function(from, to){
        sqrt( (from[1] - to[1])^2 + (from[ 2] - to[ 2])^2 )
      }
      if(fun =="e"){
        alpha  <- log(2) / span ^ beta
        fric <- function(alpha,matdist,beta){
          exp(- alpha * matdist ^ beta)
        }
      }
      if(fun == "p"){
        alpha  <- (2 ^ (1 / beta) - 1) / span
        fric <- function(alpha,matdist,beta){
          (1 + alpha * matdist) ^ (-beta)
        }
      }
      
      # Buffer limit
      gbuf <- st_buffer(ysfc, limit)
      inter <- st_intersects(gbuf, xsfc, prepared = TRUE)
      
      # data transformation
      ugeom <- matrix(unlist(ysfc), ncol = 2, byrow = TRUE)
      
      # go through each y
      l <- vector("list", nrow(ugeom))
      for (i in seq_along(l)){
        kindex <- unlist(inter[i])
        kn <- kgeom[kindex,,drop=FALSE]
        un <- ugeom[i,]
        matdist <- apply(kn, 1, eucledian_simple, un)
        un <- apply(
          X = v[kindex, , drop = FALSE], 
          MARGIN = 2,
          FUN = function(x){sum(x * fric(alpha, matdist, beta), na.rm = TRUE)}
        )
        l[[i]] <- un
      }
      unlist(l)
    }
  )
  # stop parralel
  parallel::stopCluster(cl)
  if(length(var)==1){
    pot <- as.numeric(pot)
  }else{
    pot <- matrix(pot, ncol = length(var), byrow = T, dimnames = list(NULL, var))
  }
  
  return(pot)
}