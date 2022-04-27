#' @title Test if RasterLayer
#'
#' @description Used to test if an object is of class `raster::RasterLayer`.
#'
#' @export
#' @param x   The object whose class is to be tested.
#'
#' @return logical; TRUE if class is of type `raster::RasterLayer`
#'
#' @importFrom raster extent ncol nrow ncell minValue maxValue projectRaster
#' @importFrom sp CRS proj4string
#'
is_RasterLayer <- function(x) {
  if(class(x)[[1]] == "RasterLayer") {
    raster_layer <- TRUE
  } else {
    raster_layer <- FALSE
  }
  return(raster_layer)
}
