#' @title Has Colortable?
#'
#' @export
#' @description Determines if a RasterLayer has a valid colortable defined.
#'
#' @param raster RasterLayer; The raster to be tested.
#'
#' @return Returns TRUE if the raster has a valid colortable.
#'
#' @importFrom raster raster is.factor
#'
has_colortable <- function(raster) {
  # Check inputs
  if(!is_RasterLayer(raster)) {stop("`raster` must be a raster")}

  colortable <- FALSE

  # Check if raster is a factor
  if(raster::is.factor(raster)) {
    # Check that colortable has been set
    if(!is.logical(raster@legend@colortable)) {
      # Check that colortable contains valid colors
      if(all(are_colors(raster@legend@colortable))){
        colortable <- TRUE
      }
    }
  }

  return(colortable)
}
