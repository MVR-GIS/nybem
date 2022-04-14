#' @title Set Habitat Zone
#'
#' @description Creates a habitat zone raster from the input tidal zone and
#'   salinity zones rasters.
#'
#' @export
#' @param tidal_zone    raster; A tidal zone raster.
#' @param salinity_zone raster; A salinity zone raster.
#'
#' @return A categorical habitat zone RasterLayer where:
#' 1. Upland
#' 2. Marine-Deep
#' 3. Marine-Subtidal
#' 4. Marine-Intertidal
#' 5. Estuarine-Subtidal
#' 6. Estuarine-Intertidal
#' 7. Freshwater
#'
#' @importFrom raster raster ratify levels
#'
set_habitat_zone <- function(tidal_zone, salinity_zone) {
  # Check inputs
  if(!is_RasterLayer(tidal_zone)) {stop("`tidal_zone` must be a raster")}
  if(!is_RasterLayer(salinity_zone)) {stop("`salinity_zone` must be a raster")}

  #Assign habitat zone
  habitat_zone <- ifelse(tidal_zone == 4, 1,
                  ifelse(salinity_zone == 1 &  tidal_zone == 1,  2,
                  ifelse(salinity_zone == 1 &  tidal_zone == 2,  3,
                  ifelse(salinity_zone == 1 &  tidal_zone == 3,  4,
                  ifelse(salinity_zone == 2 & (tidal_zone == 1 |
                                               tidal_zone == 2), 5,
                  ifelse(salinity_zone == 2 &  tidal_zone == 3,  6,
                  ifelse(salinity_zone == 3 & (tidal_zone == 1 |
                                               tidal_zone == 2 |
                                               tidal_zone == 3), 7, 0)))))))
  ## Assign factor labels
  # Convert the raster to a factor and create a blank raster attribute table
  habitat_zone <- raster::ratify(habitat_zone)

  # Extract the raster factor levels and write to a new data frame
  habitat_zone_rat <- levels(habitat_zone)[[1]]

  # Create a new `value` column
  habitat_zone_rat$value <- habitat_zone_rat$ID

  # Assign factor labels
  class_labels <- data.frame("ID" = c(1, 2, 3, 4, 5, 6, 7),
                         "labels" = c("Upland",
                                      "Marine-Deep", "Marine-Subtidal",
                                      "Marine-Intertidal", "Estuarine-Subtidal",
                                      "Estuarine-Intertidal", "Freshwater"))
  habitat_zone_rat <- dplyr::left_join(habitat_zone_rat,
                                       class_labels,
                                       by = "ID")

  # Use this data frame to set the levels of the raster attribute table
  levels(habitat_zone) <- habitat_zone_rat

  # Set the raster legend attributes
  habitat_zone@legend@names <- c("NoData", class_labels$labels)
  # https://colorbrewer2.org/#type=diverging&scheme=PRGn&n=7
  habitat_zone@legend@colortable <- c("#00000000", "#CCD1D1",
                                      "#762a83", "#af8dc3",
                                      "#e7d4e8", "#d9f0d3",
                                      "#7fbf7b", "#1b7837")
  return(habitat_zone)
}
