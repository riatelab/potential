#' @title Display the Density of Probability of the Spatial Interaction
#' @description Display the Density of Probability of the Spatial Interaction
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
#' @param limit maximum distance used to retrieved \code{x} points, in 
#' map units.
#' @return a plot
#' @import graphics
#' @export
#'
#' @examples
#' prob_interaction(fun = "e", span = 2000, beta = 2, limit = 4000)
#' prob_interaction(fun = "p", span = 2000, beta = 2, limit = 20000)
prob_interaction <- function(fun = "e", span, beta, limit = span * 5) {
  if (fun == "e") {
    alpha <- log(2) / span^beta
    fric <- function(alpha, matdist, beta) {
      exp(-alpha * matdist^beta)
    }
    fna <- "Exponential"
  }
  if (fun == "p") {
    alpha <- (2^(1 / beta) - 1) / span
    fric <- function(alpha, matdist, beta) {
      (1 + alpha * matdist)^(-beta)
    }
    fna <- "Pareto"
  }

  d <- seq(0, limit, length.out = 100)
  inter <- fric(alpha, d, beta)
  dp <- seq(limit, limit + (limit / 4), length.out = 25)
  interp <- fric(alpha, dp, beta)

  plot.new()
  plot.window(xlim = c(0, limit + limit / 4), ylim = c(0, 1))
  grid(ny = 4)

  points(d, inter, type = "l", lwd = 1.5)
  points(dp, interp, type = "l", col = "grey", lwd = 1.5)

  points(x = span, y = 0.5, pch = 21, bg = "red")
  text(x = span, y = 0.5, labels = "span", col = "grey40", pos = 4)

  points(x = limit, y = fric(alpha, limit, beta), pch = 21, bg = "red")
  text(x = limit, y = fric(alpha, limit, beta), labels = "limit", 
       col = "grey40", pos = 3)

  segments(
    x0 = 0, y0 = fric(alpha, limit, beta),
    x1 = limit, y1 = fric(alpha, limit, beta),
    lwd = .5, lty = 2, col = "grey40"
  )
  text(
    x = 0, y = fric(alpha, limit, beta),
    labels = signif(fric(alpha, limit, beta), digits = 2),
    cex = .8, adj = c(0, 0), col = "grey40"
  )


  axis(1, lwd = 0, font.axis = 2)
  axis(2, lwd = 0, font.axis = 2, at = seq(0, 1, length.out = 5))

  title(xlab = "Distance", col.lab = "red3")
  title(ylab = "Interaction intensity", col.lab = "red3")
  title(
    main = "Density of Probability of the Spatial Interaction",
    col.main = "red3", adj = 0
  )
  mtext(
    side = 1, line = 4, adj = 0,
    text = paste0(
      fna, " function: span = ",
      span, ", beta = ", beta,
      ", limit = ", limit
    )
  )
}
