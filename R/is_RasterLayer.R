#' @title Test if RasterLayer
#'
#' @description Used to test if an object is of class `raster::RasterLayer`.
#'
#' @export
#' @param x   The object whose class is to be tested.
#'
#' @return logical; TRUE if class is of type `raster::RasterLayer`
#'
#' @importFrom raster raster values getValues setValues minValue maxValue
#'                    crop extent ncol nrow ncell
#'                    projectRaster proj4string
#' @importFrom sp CRS
#'
is_RasterLayer <- function(x) {
  if(class(x)[[1]] == "RasterLayer") {
    raster_layer <- TRUE
  } else {
    raster_layer <- FALSE
  }
  return(raster_layer)
}
