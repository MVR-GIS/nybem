#' @title Test if RasterLayer
#'
#' @description Used to test if an object is of class `raster::RasterLayer`.
#'
#' @param x   The object whose class is to be tested.
#'
#' @return logical; TRUE if class is of type `raster::RasterLayer`
#'
is_RasterLayer <- function(x) {
  if(class(x)[[1]] == "RasterLayer") {
    raster_layer <- TRUE
  } else {
    raster_layer <- FALSE
  }
  return(raster_layer)
}
