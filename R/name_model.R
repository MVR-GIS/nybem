#' @title Name Raster HSI Model
#'
#' @description Assign a name to a raster HSI model. Used for processing
#' individual models into alternatives analysis for a study area.
#'
#' @export
#' @param hsi_model   raster; A raster HSI model.
#' @param name_name   character; The name of the model.
#'
#' @return A raster with the name assigned.
#'
name_model <- function(hsi_model, model_name) {
  # Check inputs
  if(!is_RasterLayer(hsi_model)) {stop("hsi_model must be a raster")}
  if(!is.character(model_name)) {stop("model_name must be a character")}

  # Clean up name
  model_name <- stringr::str_replace_all(model_name, "\\.| ", "_")

  # Assign model name
  names(hsi_model) <- model_name

  return(hsi_model)
}
