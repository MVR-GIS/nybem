#' @title Summarize HSI Models
#'
#' @description Display a brief set of result summaries for each HSI model.
#'
#' @export
#' @param hsi_models  list; A named list of HSI model rasters.
#' @param extent      raster::Extent; An extent object used to crop rasters.
#'
#' @return Returns a histogram and tmap for each of the HSI models.
#'
#' @importFrom purrr map
#' @importFrom raster crop
#'
model_summary <- function(hsi_models, extent) {
  # Check inputs
  if(!is.list(hsi_models)) {stop("hsi_model must be a list")}
  if(!class(extent)[1] == "Extent") {
    stop("`extent` must be a raster Extent object")}
  if(!all(unlist(map(hsi_models, is_RasterLayer)))) {
    stop("hsi_model must be a list of rasters")}

  map(hsi_models,
      function(x) {
        # Crop to extent
        ras_ex <- raster::crop(x, extent)

        # Visualize cropped model
        raster::hist(ras_ex)
        nybem::raster_fig(ras_ex, extent, names(ras_ex))
      }
  )
}
