test_point <- function(x, id){
  
  if (!inherits(x = x, what = "sf")) {
    stop(paste0(id, " is not an sf object."), 
         call. = FALSE)
  }
  
  type <- sf::st_geometry_type(x, by_geometry = FALSE)
  if (type != "POINT") {
    stop(paste0('"', id, '" geometry should be of type POINT.'), 
         call. = FALSE)    
  }
}