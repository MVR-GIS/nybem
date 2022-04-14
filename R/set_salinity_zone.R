#' @title Set Salinity Zone
#'
#' @description Creates a Salinity Zone raster from the input salinity raster
#' applying the input threshold parameters.
#'
#' @export
#' @param salinity               raster; A raster of salinity measurements or
#'                               estimates.
#' @param estuarine_marine_break numeric; The salinity threshold between
#'                               estuarine and marine zones.
#' @param fresh_estuarine_break  numeric; The salinity threshold between
#'                               fresh and estuarine zones.
#'
#' @return A categorical salinity zone RasterLayer where:
#' 1. Marine
#' 2. Estuarine
#' 3. Fresh
#'
#' @importFrom raster raster ratify levels
#' @importFrom dplyr left_join
#'
set_salinity_zone <- function(salinity,
                              estuarine_marine_break,
                              fresh_estuarine_break){
  # Check inputs
  if(!is_RasterLayer(salinity)) {stop("`salinity` must be a raster")}
  if(!is.numeric(estuarine_marine_break)) {
    stop("`estuarine_marine_break` must be numeric")}
  if(!is.numeric(fresh_estuarine_break)) {
    stop("`fresh_estuarine_break` must be numeric")}

  #Assign salinity zone
  salinity_zone <- ifelse(salinity >= estuarine_marine_break, 1,
                   ifelse(salinity <  estuarine_marine_break &
                          salinity >= fresh_estuarine_break, 2,
                   ifelse(salinity <  fresh_estuarine_break, 3, 0)))

  ## Assign factor labels
  # Convert the raster to a factor and create a blank raster attribute table
  salinity_zone <- raster::ratify(salinity_zone)

  # Extract the raster factor levels and write to a new data frame
  salinity_zone_rat <- levels(salinity_zone)[[1]]

  # Create a new `value` column
  salinity_zone_rat$value <- salinity_zone_rat$ID

  # Assign factor labels
  class_labels <- data.frame("ID" = c(1, 2, 3),
                             "labels" = c("Marine", "Estuarine", "Fresh"))
  salinity_zone_rat <- dplyr::left_join(salinity_zone_rat,
                                     class_labels,
                                     by = "ID")

  # Use this data frame to set the levels of the raster attribute table
  levels(salinity_zone) <- salinity_zone_rat

  # Set the raster legend attributes
  salinity_zone@legend@names <- c("NoData", class_labels$labels)
  # https://colorbrewer2.org/#type=diverging&scheme=BrBG&n=4
  salinity_zone@legend@colortable <- c("#00000000",
                                       "#018571", "#80cdc1", "#dfc27d")

  return(salinity_zone)
}
