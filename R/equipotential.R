#' @title Create Polygons of Equipotential
#' @name equipotential
#' @description
#' This function creates polygons of equipotential from a regular grid of 
#' potential points. 
#' @param x an sf object of regularly spaced points. It must contain "X", "Y" 
#' and "OUTPUT" fields.
#' @param nclass a number of class.
#' @param breaks a vector of break values.
#' @param mask an sf object of polygons or multipolygons. /code{mask} is  used 
#' to clip polygons of contours equipotential.
#' @param xcoords name of the X coordinates field in \code{x}.
#' @param ycoords name of the Y coordinates field in \code{x}.
#' @param var name of the OUTPUT field in \code{x}.
#' @return The output is an sf object of polygons. The data frame contains four 
#' fields: id (id of each polygon), min and max (minimum and maximum breaks of 
#' the polygon) and center (central values of classes).
#' @importFrom sf st_as_sf st_crs st_bbox st_cast st_sf st_sfc st_intersection
#' st_union st_agr<- st_collection_extract st_make_valid
#' @importFrom isoband isobands iso_to_sfg
#' @importFrom methods is
#' @examples
#' library(sf)
#' pot <- potential(
#'   x = n3_pt, var = "POP19",
#'   fun = "e", span = 200000, beta = 2,
#'   res = 200000, mask = n3_poly
#' )
#' equipot <- equipotential(pot, mask = n3_poly)
#' plot(equipot['center'], pal = hcl.colors(nrow(equipot), "cividis"))
#' @export
equipotential <- function(x,
                          nclass = 8,
                          breaks,
                          mask,
                          xcoords = "COORDX",
                          ycoords = "COORDY",
                          var = "OUTPUT") {
  # get initial min and max values
  vmin <- min(x[[var]], na.rm = TRUE)
  vmax <- max(x[[var]], na.rm = TRUE)

  if (missing(breaks)) {
    breaks <- seq(from = vmin, to = vmax, length.out = (nclass + 1))
  } else {
    breaks <- sort(unique(c(vmin, breaks[breaks > vmin & breaks < vmax], vmax)))
  }

  m <- matrix(
    data = x[[var]], nrow = length(unique(x[[xcoords]])),
    dimnames = list(unique(x[[xcoords]]), unique(x[[ycoords]]))
  )

  lev_low <- breaks[1:(length(breaks) - 1)]
  lev_high <- breaks[2:length(breaks)]
  raw <- isobands(
    x = as.numeric(rownames(m)),
    y = as.numeric(colnames(m)), z = t(m),
    levels_low = lev_low,
    levels_high = c(lev_high[-length(lev_high)], vmax + 1e-10)
  )

  bands <- iso_to_sfg(raw)
  iso <- st_sf(
    id = 1:length(bands),
    min = lev_low,
    max = lev_high,
    geometry = st_sfc(bands),
    crs = st_crs(x)
  )
  iso$center <- iso$min + (iso$max - iso$min) / 2



  st_geometry(iso) <- st_make_valid(st_geometry(iso))


  if (methods::is(st_geometry(iso), "sfc_GEOMETRY")) {
    st_geometry(iso) <- st_collection_extract(st_geometry(iso), "POLYGON")
  }

  if (!missing(mask)) {
    if (is(mask, "Spatial")) {
      mask <- st_as_sf(mask)
    }
    st_agr(iso) <- "constant"
    iso <- st_cast(st_intersection(x = iso, y = st_union(st_geometry(mask))))
  }
  return(iso)
}
