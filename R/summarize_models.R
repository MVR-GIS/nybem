#' @title Summarize HSI Models by Polygons
#'
#' @description Summarizes the list of models by each polygon. This function
#' makes a call to the `summarize_by_poly` function for each model in the
#' `hsi_models` list. This function assembles those results into a single
#' output data frame.
#'
#' @export
#' @param hsi_models  list; A named list of recoded HSI models created using
#'                    the `recode_models` function.
#' @param polys       sf; A polygons sf object.
#' @param progress    logical; TRUE displays a progress bar during processing.
#'
#' @return A data frame of model summaries containing the following fields
#' for each model:
#'   * ID - Polygon unique identifier. Derived from polys' row.names.
#'   * hu_<model_name> - HSI score mean for the polygon. Model name is derived
#'                       from the raster data names attribute via names().
#'   * count_<model_name> - Count of cells for the polygon.
#'   * acres_<model_name> - Area in acres for the polygon.
#'
#' @importFrom purrr map
#' @importFrom dplyr bind_cols select
#'
summarize_models <- function(hsi_models, polys, progress = FALSE) {
  feature_summaries <- hsi_models %>%
    purrr::map(~ nybem::summarize_by_poly(.x, polys,
                                          progress = progress)) %>%
    dplyr::bind_cols() %>%
    dplyr::select(-starts_with("ID"))
}
