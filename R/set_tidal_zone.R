#' @title Create a Tidal Zone Raster
#'
#' @description Creates a tidal zone raster from the input rasters.
#'
#' @export
#' @param bed_elevation  RasterLayer; A bed elevation raster.
#' @param MLLW           RasterLayer; A Mean Higher High Water (MHHW)
#'                       raster.
#' @param MHHW           RasterLayer; A Mean Lower Low Water (MLLW)
#'                       raster.
#'
#' @details The z-values of all input rasters must be expressed in the same
#' vertical datum (i.e., National Tidal Datum Epoch, NAVD88).
#'
#' @return A categorical RasterLayer where:
#'  1. Deep
#'  2. Subtidal
#'  3. Intertidal
#'  4. Upland
#'
#' @importFrom raster raster ratify levels
#'
set_tidal_zone <- function(bed_elevation, MLLW, MHHW) {
  # Check inputs
  if(!is_RasterLayer(bed_elevation)) {stop("`bed_elevation` must be a raster")}
  if(!is_RasterLayer(MLLW)) {stop("`MLLW` must be a raster")}
  if(!is_RasterLayer(MHHW)) {stop("`MHHW` must be a raster")}

  # Assign tidal zone values
  tidal_zone <- nybem::ifelse(bed_elevation <= -2,   1,          # Deep
                nybem::ifelse(bed_elevation <= MLLW, 2,          # Subtidal
                nybem::ifelse(bed_elevation <= MHHW, 3,          # Intertidal
                nybem::ifelse(bed_elevation >  MHHW, 4, 0))))    # Upland

  ## Assign factor labels
  # Convert the raster to a factor and create a blank raster attribute table
  tidal_zone <- raster::ratify(tidal_zone)

  # Extract the raster factor levels and write to a new data frame
  tidal_zone_rat <- levels(tidal_zone)[[1]]

  # Create a new `value` column
  tidal_zone_rat$value <- tidal_zone_rat$ID

  # Assign factor labels
  class_labels <- data.frame("ID" = c(1, 2, 3, 4),
                             "labels" = c("Deep", "Subtidal",
                                          "Intertidal", "Upland"))
  tidal_zone_rat <- dplyr::left_join(tidal_zone_rat,
                                     class_labels,
                                     by = "ID")

  # Use this data frame to set the levels of the raster attribute table
  levels(tidal_zone) <- tidal_zone_rat

  # Set the raster legend attributes
  tidal_zone@legend@names <- c("NoData", class_labels$labels)
  # https://colorbrewer2.org/#type=qualitative&scheme=Paired&n=4
  tidal_zone@legend@colortable <- c("#00000000",                  # No Data
                                    "#1f78b4",                    # Deep
                                    "#a6cee3",                    # Subtidal
                                    "#33a02c",                    # Intertidal
                                    "#b2df8a")                    # Upland

  return(tidal_zone)
}
