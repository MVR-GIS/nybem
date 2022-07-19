#' @title Summarize Raster by Polygon
#'
#' @description Summarize the raster HSI model by polygon.
#'
#' @export
#' @param hsi_model   raster; A raster HSI model.
#' @param polys       sf; A polygon used to derive be summaries from the
#'                    raster.
#' @param progress    logical; TRUE displays a progress bar during processing
#'
#' @return A data frame containing polygons summarized by the following fields:
#'   * ID - Polygon unique identifier.
#'   * hu_<model_name> - HSI score mean for the polygon.
#'   * count_<model_name> - Count of cells for the polygon.
#'   * acres_<model_name> - Area in acres for the polygon.
#'
#' @importFrom rlang enquo !! := sym
#' @importFrom exactextractr exact_extract
#' @importFrom terra rast linearUnits
#' @importFrom dplyr rename mutate relocate
#' @importFrom magrittr %>%
#'
summarize_by_poly <- function(hsi_model, polys, progress = TRUE) {
  # Check inputs
  if(!is_RasterLayer(hsi_model)) {stop("hsi_model must be a raster")}
  if(!class(polys)[1] == "sf") {stop("polys must be an sf object")}
  if(!is.logical(progress) == TRUE) {stop("progress must be logical")}

  # Construct and enquote field names
  model <- names(hsi_model)
  hu    <- paste0("hu_", model)
  cnt   <- paste0("count_", model)
  area  <- paste0("acres_", model)
  model_name <- enquo(model)
  hu_field   <- enquo(hu)
  cnt_field  <- enquo(cnt)
  area_field <- enquo(area)

  # Summarize model for each poly
  sum_df <- exactextractr::exact_extract(hsi_model, polys,
                                         fun = c("mean", "count"),
                                         progress = progress)

  # Calculate cell area in m sq
  terra_rast <- terra::rast(hsi_model)
  cell_size_m  <- terra::linearUnits(terra_rast)
  cell_area_m2 <- cell_size_m^2

  # Rename fields and calculate area in acres
  sum_df <- sum_df %>%
    rename(!!hu_field := mean) %>%
    rename(!!cnt_field := count) %>%
                                         # 1 sq m = 0.000247105 acres
    mutate(!!area_field := (!!sym(cnt) * cell_area_m2) * 0.000247105) %>%
    mutate(ID = as.numeric(row.names(.))) %>%
    relocate(ID, .before = 1)

  return(sum_df)
}
