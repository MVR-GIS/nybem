#' @title Write Continuous Raster TIFF file
#'
#' @description Writes a continuous raster file in .tiff format with the
#' correct raster attribute table information to be read by ESRI ArcGIS
#' products.
#'
#' @export
#' @param cont_raster  raster::Raster*; A continuous raster.
#' @param out_dir      character; A path to a folder.
#' @param out_name     character; A file name.
#'
#' @return This function is used for the side-effect of writing a continuous
#' raster to a file.
#'
#' @importFrom raster writeRaster
#' @importFrom tools file_path_sans_ext
#'
write_cont_tiff <- function(cont_raster, out_dir, out_name) {
  # Check inputs
  if(!is_RasterLayer(cont_raster)) {stop("cont_raster must be a raster")}
  if(!file.exists(out_dir)) {stop("out_dir does not exist")}

  # Set output names
  out_name_no_ext <- tools::file_path_sans_ext(out_name)
  out_name_tif   <- paste0(out_name_no_ext, ".tif")
  out_tif <- file.path(out_dir, out_name_tif)

  # https://gdal.org/drivers/raster/gtiff.html
  writeRaster(cont_raster,
              filename = out_tif,
              format="GTiff",
              options = c("COMPRESS=LZW",
                          "PREDICTION=3",
                          "NUM_THREADS=ALL_CPUS",
                          "TFW=YES"),
              overwrite=TRUE)
}
