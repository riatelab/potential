#' library(sf)
#' library(potential)
#' library(cartography)
#' x <- n3_pt[substr(n3_pt$ID,1,3) %in% c( "FRJ"), ]
#' x_poly <- n3_poly[substr(n3_poly$ID,1,3) %in% c( "FRJ"),]
#' x$POP19 <- round(x$POP19 / 1000, 0)
#' x <- n3_pt
#' 
#' y <- create_grid(x = x, res = 5000)
#' d <- create_matrix(x, y)
#' r <- prepare_data2(x = x, y = y, d = d)
#' matdens <- interact_density(
#'   d = r$d,
#'   fun = "e", beta = 2, span = 25000
#' )
#' m2 <- matdens * r$x$POP19
#' 
#' m <- matrix(data = NA, nrow = nrow(r$x), ncol = nrow(r$y))
#' for(i in 1:nrow(r$x)){
#'   for (j in 1:nrow(r$y)){
#'     m[i, j] <- get_angle(r$x[i,"xy"], r$y[j,"xy"])
#'   }
#' }
#' 
#' get_angle <- function(x, y){
#'   atan2(y = (y[2] - x[2]), x = (y[1] - x[1]))
#' }
#' 
#' 
#' ewv <- m2 * sin(m)
#' nsv <- m2 * cos(m)
#' 
#' ewvws <- colMeans(ewv)
#' nsvws <- colMeans(nsv)
#' 
#' intens <- sqrt(ewvws^2 + nsvws^2)
#' 
#' int <- 2 * intens / max(intens)
#' 
#' wwdr <- atan2(ewvws,nsvws)
#' 
#' # wwdr[nsvws<0] <- wwdr[nsvws<0] + pi
#' 
#' wwdd <- wwdr * 180 / pi
#' 
#' y$an <- wwdd
#' 
#' y$pot <- potential(x, y, d, "POP19", "e", 25000, 2 )
#' z <- equipotential(y, var = "pot", nclass = 20, mask  =n3_poly)
#' par(mar = c(0,0,0,0), family = "JetBrains Mono")
#' mapsf::mp_map_t(z, "center", pal = "Reds", border = NA, leg_pos = NA)
#' mapsf::mp_map_p(x, "POP19", col = NA, border = "white", leg_pos = NA)
#' for(i in 1:nrow(y)){
#'   text(x = y$COORDX[i], y = y$COORDY[i],
#'        labels = "<-", srt = y$an[i], cex = int[i])
#' }
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' # Internal functions
#' #' @importFrom sf st_drop_geometry
#' prepare_data2 <- function(x, y, d) {
#'   d <- use_matrix(d = d, x = x, y = y)
#'   x$xy <-  st_coordinates(x)
#'   y$xy <-  st_coordinates(y)
#' 
#'   x <- st_drop_geometry(x)
#'   y <- st_drop_geometry(y)
#' 
#' 
#' 
#'   return(list(x = x, y = y, d = d))
#' }
#' 
#' 
#' use_matrix <- function(d, x, y) {
#'   i <- factor(row.names(x), levels = row.names(x))
#'   j <- factor(row.names(y), levels = row.names(y))
#'   d <- d[levels(i), levels(j)]
#'   return(round(d, digits = 8))
#' }
#' 
#' 
#' interact_density <- function(d, fun, beta, span) {
#'   if (fun == "p") {
#'     alpha <- (2^(1 / beta) - 1) / span
#'     matDens <- (1 + alpha * d)^(-beta)
#'   } else if (fun == "e") {
#'     alpha <- log(2) / span^beta
#'     matDens <- exp(-alpha * d^beta)
#'   }
#'   return(matDens)
#' }
#' 
#' compute_potentials <- function(x, matdens, var) {
#'   pot <- apply(x * matdens, 2, sum, na.rm = TRUE)
#' }
