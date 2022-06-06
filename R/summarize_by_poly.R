#' @title Summarize Raster by Polygon
#'
#' @description Summarize the raster HSI model by polygon.
#'
#' @export
#' @param hsi_model   raster; A raster HSI model.
#' @param polys       sf; A polygon used to derive be summaries from the
#'                    raster.
#'
#' @return A data frame containing polygons summarized by the following fields:
#'   * ID - Polygon unique identifier.
#'   * hu_<model_name> - HSI score mean for the polygon.
#'   * count_<model_name> - Count of cells for the polygon.
#'   * acres_<model_name> - Area in acres for the polygon.
#'
#' @importFrom rlang enquo !! := sym
#' @importFrom raster extract area
#' @importFrom terra rast linearUnits
#' @importFrom dplyr rename mutate inner_join
#' @importFrom magrittr %>%
#'
summarize_by_poly <- function(hsi_model, polys) {
  # Check inputs
  if(!is_RasterLayer(hsi_model)) {stop("hsi_model must be a raster")}
  if(!class(polys)[1] == "sf") {stop("polys must be an sf object")}

  # Construct and enquote field names
  model <- names(hsi_model)
  hu    <- paste0("hu_", model)
  cnt   <- paste0("count_", model)
  area  <- paste0("acres_", model)
  model_name <- enquo(model)
  hu_field   <- enquo(hu)
  cnt_field  <- enquo(cnt)
  area_field <- enquo(area)

  # Summarize model HSI for each poly
  hu_df <- raster::extract(hsi_model, polys,
                           fun = mean,
                           na.rm = TRUE,
                           df = TRUE)

  # Reclassify HSI model values to 1 for counting
  hsi_model_1 <- raster::reclassify(hsi_model,
                                    cbind(-Inf, Inf, 1))

  # Calculate cell area in sq m
  cell_size_m <- terra::linearUnits(terra::rast(hsi_model))
  cell_area <- cell_size_m^2

  # Summarize modeled area for each poly
  num_cells_df <- raster::extract(hsi_model_1, polys,
                                  fun = sum,
                                  na.rm = TRUE,
                                  df = TRUE)

  # Calculate modeled area
  num_cells_df <- num_cells_df %>%
    rename(!!cnt_field := !!model_name) %>%
                                               # 1 sq m = 0.000247105 acres
    mutate(!!area_field := (!!sym(cnt) * cell_area) * 0.000247105)

  # Join HSI and Area dfs
  hu_df <- hu_df %>%
    rename(!!hu_field := !!model_name) %>%
    inner_join(num_cells_df, by = "ID")

  return(hu_df)
}
