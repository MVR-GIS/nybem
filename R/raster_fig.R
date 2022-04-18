#' @title Create a Raster Figure
#'
#' @description Creates a raster figure cropped to the specified extent
#'
#' @export
#' @param ras     raster::RasterLayer; A raster object.
#' @param extent  raster::Extent; An extent object used to crop the raster.
#' @param title   character; A title for the raster legend.
#'
#' @return A tmap object
#'
#' @importFrom raster raster crop
#' @importFrom tmap tmap_options tm_shape tm_raster tm_legend
#'
raster_fig <- function(ras, extent,
                       title = "Title") {
  # Check inputs
  if(!is_RasterLayer(ras)) {stop("`ras` must be a raster")}
  if(!class(extent)[1] == "Extent") {
    stop("`extent` must be a raster Extent object")}
  if(!is.character(title)) {stop("`title` must be character")}

  # Set tmap options
  tmap_options(max.raster = c(plot = 1e+07, view = 1e+06))

  # Crop to extent
  ras_ex <- raster::crop(ras, extent)

  if(is_cat_raster(ras_ex)) {
    tm_shape(ras_ex) +
      tm_raster(style = "cat",
                labels = ras_ex@legend@names[-1],
                title = "Tidal Zones") +
      tm_legend(outside = TRUE)
  } else {
    tm_shape(ras_ex) +
      tm_raster(style = "cont",
                title = title) +
      tm_legend(outside = TRUE)
  }
}
