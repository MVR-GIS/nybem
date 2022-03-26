#' @title Calculate Habitat Suitability Index (HSI)
#'
#' @description Calculates a habitat suitability index for the input
#'   suitability indices using the specified method.
#'
#' @export
#' @param si_list   list; A list of suitability indices.
#' @param method    character; The method for calculating the suitability
#'                  index. Must be one of "mean",
#'
#' @return An HSI object matching the data format of the input suitability
#'   indices.
#'
HSIcalc <- function(si_list, method = "mean") {
  # si_list parameter must be list to preserve metric data type
  if(!is.list(si_list)) {stop("si_list must be a list")}
  if(!(method %in% c("mean"))) {
    stop("method must be one of 'mean', ")}

  # Check input for rasters
  raster_input <- any(unlist(lapply(si_list, is_RasterLayer)))

  # Create a raster brick
  if(raster_input) {
    si <- raster::brick(si_list)
  }

  # Convert list to vector for non-rasters
  if(!raster_input) {
    si <- unlist(si_list)
  }

  if(method == "mean") {
    if(!raster_input) {
      hsi_score <- mean(si, na.rm = TRUE)
    }

    if(raster_input) {
      hsi_score <- raster::calc(si, mean)
    }
  }

  return(hsi_score)
}
