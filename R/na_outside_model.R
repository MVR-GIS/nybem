#' @title Set NA Outside Modeled Area
#'
#' @description HSI values outside the modeled habitat zone were set to -1 to
#' distinguish areas inside the study area, but outside the modeled habitat
#' zone. This function is used for summarizing modeled habitat.
#'
#' @export
#' @param hsi_model  raster; A raster containing HSI modeled values.
#'
#' @return  A raster with unmodeled HSI cells originally set to -1, converted
#' to NA for summarization.
#'
na_outside_model <- function(hsi_model) {
  # Check inputs
  if(!is_RasterLayer(hsi_model)) {stop("hsi_model must be a raster")}

  model_with_na <- raster::reclassify(hsi_model,
                                      cbind(-Inf, 0, NA),
                                      right = FALSE)
  return(model_with_na)
}
