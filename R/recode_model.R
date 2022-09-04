#' @title Recode HSI Models.
#'
#' @description Recodes the -1 raster values used to denote the area within
#' the study area, but outside the model habitat zone to NA. HSI models
#' assign areas inside each model's habitat zone an HSI value of 0-1,
#' while areas outside a model's habitat zone are assigned a value of -1 to
#' distinguish it from areas outside the study area which are assigned the
#' value NoData. Some model summary functions require that these placeholder
#' values (-1) are converted to NA.
#'
#' @export
#' @param model_list  list; A named list of HSI model rasters.
#'
#' @return A named list of HSI models whose raster have had the -1 values
#' recoded to NA.
#'
#' @importFrom purrr map
#'
recode_models <- function(model_list) {
  # Assign names to the input HSI model raster objects
  hsi_models <- purrr::map(names(model_list),
                           ~ nybem::name_model(model_list[[.x]], .x))
  # Name the `hsi_model` list elements
  names(hsi_models) <- names(model_list)

  # Replace HSI model -1 values (study area outside habitat zone) with NA
  hsi_models_recode <- purrr::map(hsi_models, nybem::na_outside_model)

  return(hsi_models_recode)
}
