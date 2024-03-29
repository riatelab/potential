% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_matrix.R
\name{create_matrix}
\alias{create_matrix}
\title{Create a Distance Matrix Between Two Spatial Objects}
\usage{
create_matrix(x, y, checksize = TRUE, longlat = FALSE)
}
\arguments{
\item{x}{an sf object (POINT), rows of the distance matrix, row names are 
used as row names of the matrix.}

\item{y}{an sf object (POINT), columns of the distance matrix, row names 
are used as column names of the matrix.}

\item{checksize}{if FALSE, bypass the distance matrix size control
(see Details).}

\item{longlat}{if FALSE, the Euclidean distance is used, if TRUE Great Circle
(WGS84 ellipsoid) distance is used.}
}
\value{
A distance matrix, row names are \code{x} row names, column
names are \code{y} row names.
}
\description{
This function creates a distance matrix between two
spatial objects.
}
\details{
The function returns a full matrix of distances in meters.
If the matrix to compute is too large (more than 100,000,000 cells, more than
10,000,000 origins or more than 10,000,000 destinations)
the function may sends a message to warn users about the amount of
RAM mobilized.
}
\examples{
g <- create_grid(x = n3_poly, res = 200000)
mat <- create_matrix(x = n3_pt, y = g)
mat[1:5, 1:5]
}
