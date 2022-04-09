#' @title Write Catagorical Raster TIFF file
#'
#' @description Writes a catagorical raster file in .tiff format with the
#' correct raster attribute table information to be read by ESRI ArcGIS
#' products.
#'
#' @export
#' @param cat_raster   raster::Raster*; A catagorical raster.
#' @param out_dir      character; A path to a folder.
#' @param out_name     character; A file name.
#'
#' @return This function is used for the side-effect of writing a catagorical
#' raster to a file.
#'
#' @importFrom raster writeRaster
#' @importFrom foreign write.dbf
#'
write_cat_tif <- function(cat_raster, out_dir, out_name) {
  # Check inputs
  if(!is_RasterLayer(cat_raster)) {stop("`cat_raster` must be a raster")}

  # Set output names
  out_tif <- file.path(out_dir, out_name)
  out_dbf <- file.path(out_dir, paste0(out_name, ".tif.vat.dbf"))
  out_cpg <- file.path(out_dir, paste0(out_name, ".tif.vat.cpg"))
  out_clr <- file.path(out_dir, paste0(out_name, ".tif.vat.cpg"))

  # Save the raster
  raster::writeRaster(cat_raster,
                      filename = out_tif,
                      format = "GTiff",
                      datatype = "INT1U",
                      options = c("COMPRESS=LZW",
                                  "PREDICTION=3",
                                  "NUM_THREADS=ALL_CPUS",
                                  "TFW=YES"),
                      overwrite = TRUE
  )

  # Save the raster attribute table
  rat <- levels(cat_raster)[[1]]
  foreign::write.dbf(rat, out_dbf)

  # Save the cpg file
  fileConn<-file(out_cpg)
  writeLines(c("UTF-8"), fileConn)
  close(fileConn)
}
