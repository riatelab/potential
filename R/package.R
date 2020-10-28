#' @title Implementation of the Potential Model
#' @name potential-package
#' @rdname potential-package
#' @description This package provides functions to compute potential model as
#' defined by Stewart (1941) <doi:10.1126/science.93.2404.89>. Several options
#' are available to customize the model, such as the possibility to fine-tune
#' the distance friction functions or to use custom distance matrices. Some
#' computations are parallelized to improve their efficiency.
#' @docType package
NULL



#' @title Points and Polygons Layers of European Statistical Units (NUTS3)
#' @description
#'
#' n3_pt (POINTS) and n3_poly (MULTIPOLYGONS) are sf objects of
#' 1506 NUTS3 statistical units of continental Europe.
#'
#' Population dataset (2019 an 2018 total population) downloaded on the Eurostat
#' website (05/10/2020) from the "demo_r_pjanaggr3" dataset
#' (last update: 16/06/2020).
#'
#' Geometries are downloaded from the GISCO website 
#' (NUTS3 - 2016 - 1:60 Million)
#'
#' When data from this package is used in any printed or electronic
#' publication, in addition to any other provisions applicable to the whole
#' Eurostat website, data source will have to be acknowledged in the legend of
#' the map and in the introductory page of the publication with the following
#' copyright notice:
#' "© EuroGeographics for the administrative boundaries and © Eurostat
#' for data".
#'
#' @name n3_pt
#'
#' @usage data(nuts3)
#' @docType data
"n3_pt"


#' @title Points and Polygons Layers of European Statistical Units (NUTS3)
#' @description
#'
#' n3_pt (POINTS) and n3_poly (MULTIPOLYGONS) are sf objects of
#' 1506 NUTS3 statistical units of continental Europe.
#'
#' Population dataset (2019 an 2018 total population) downloaded on the Eurostat
#' website (05/10/2020) from the "demo_r_pjanaggr3" dataset
#' (last update: 16/06/2020).
#'
#' Geometries are downloaded from the GISCO website 
#' (NUTS3 - 2016 - 1:60 Million)
#'
#' When data from this packgage is used in any printed or electronic
#' publication, in addition to any other provisions applicable to the whole
#' Eurostat website, data source will have to be acknowledged in the legend of
#' the map and in the introductory page of the publication with the following
#' copyright notice:
#' "© EuroGeographics for the administrative boundaries and © Eurostat
#' for data".
#'
#' @name n3_poly
#'
#' @usage data(nuts3)
#' @docType data
"n3_poly"
