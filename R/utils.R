# Internal functions

prepdata <- function(x, y, d, bypassctrl, longlat, mask, 
                     res){
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
    y <- create_grid(x = mask, resolution = res) 
    d <- create_matrix(x = x, y = y, bypassctrl = bypassctrl, 
                       longlat = longlat) 
  }
  return(list(x = x, y = y, d = d))
}


use_matrix <- function(d, x, y){
  i <- factor(row.names(x), levels = row.names(x))
  j <- factor(row.names(y), levels = row.names(y))
  d <- d[levels(i), levels(j)]
  return(round(d, digits = 8))
}

ComputeInteractDensity <- function(d, fun, beta, span)
{
  if(fun == "p") {
    alpha  <- (2 ^ (1 / beta) - 1) / span
    matDens <- (1 + alpha * d) ^ (-beta)
  } else if(fun == "e") {
    alpha  <- log(2) / span ^ beta
    matDens <- exp(- alpha * d ^ beta)
  } else {
    stop("Please choose a valid interaction function argument (fun)")
  }
  matDens <- round(matDens, digits = 8)
  return(matDens)
}

ComputeOpportunity <- function(x, matdens, var)
{
  matOpport <- x[[var]] * matdens
  return(round(matOpport, digits = 8))
}

ComputePotentials <- function(y, matopport)
{
  y$OUTPUT <- apply(matopport, 2, sum, na.rm = TRUE)
  return(y)
}






