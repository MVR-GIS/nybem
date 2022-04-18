#' @title Is Catagorical Raster?
#'
#' @export
#' @param ras  raster; The raster layer to test.
#'
#' @return logical; TRUE if class is of type `raster::RasterLayer` and a
#' factor.
#'
#' @importFrom raster raster is.factor
#'
is_cat_raster <- function(ras) {
  # Check inputs
  if(!is_RasterLayer(ras)) {stop("`ras` must be a raster")}

  cat_raster <- FALSE

  # Check if raster is a factor
  if(raster::is.factor(ras)) { cat_raster <- TRUE}

  return(cat_raster)
}
