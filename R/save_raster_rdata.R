#' @title Save raster to RData file
#'
#' @description Used to ensure that raster data is first loaded into memory so
#' that raster data is saved into the .RData object and not left in a
#' referenced temp file.
#'
#' @export
#' @param raster  RasterLayer; The raster layer to be saved.
#' @param file    a (writable binary-mode) connection or the name of the file
#'                where the data will be saved (when tilde expansion is done).
#'
#' @details Using `save()` to write rasters to .RData files can produce
#' unexpected results. Large RasterLayers are not loaded into memory, but
#' contain a pointer to a file storing the data. `save()` then only stores a
#' pointer to the temp file holding the data which may not be available later.
#' This function loads on-disk RasterLayers into memory to ensure the data is
#' stored in the .RData file.
#'
#' @return None. Saves .RData file to disk.
#'
save_raster_rdata <- function(raster, file) {
  if(!is_RasterLayer(raster)) {stop("`raster` must be a raster")}

  # Ensure raster is loaded into memory
  if(raster::fromDisk(raster)) {
    raster <- raster::readAll(raster)
  }

  # Save in memory raster
  if(raster::inMemory(raster)) {
    save(raster, file = file)
  }
}
