#' @title Summarize Raster by Polygon
#'
#' @description Summarize the raster HSI model by polygon.
#'
#' @export
#' @param hsi_model   raster; A raster HSI model.
#' @param polys       sf; A polygon used to derive be summaries from the
#'                    raster.
#' @param progress    logical; TRUE displays a progress bar during processing.
#'
#' @return A data frame containing polygons summarized by the following fields:
#'   * ID - Polygon unique identifier. Derived from polys' row.names.
#'   * hsi_<model_name>   - HSI score mean for the polygon. Model name is
#'                          derived from the raster data names attribute via
#'                          names().
#'   * count_<model_name> - Count of cells for the polygon.
#'   * acres_<model_name> - Area in acres for the polygon.
#'   * hu_<model_name> -    Habitat Units (HU) calculated by multiplying HSI
#'                          by acres.
#'
#' @importFrom rlang enquo !! := sym
#' @importFrom exactextractr exact_extract
#' @importFrom raster xres yres
#' @importFrom terra rast linearUnits
#' @importFrom dplyr rename mutate relocate
#' @importFrom magrittr %>%
#'
summarize_by_poly <- function(hsi_model, polys, progress = TRUE) {
  # Check inputs
  if(!is_RasterLayer(hsi_model)) {stop("hsi_model must be a raster")}
  if(!class(polys)[1] == "sf") {stop("polys must be an sf object")}
  if(st_crs(hsi_model) != st_crs(polys)) {
    stop("hsi_model and polys coordinate reference systems must match")}
  if(!is.logical(progress) == TRUE) {stop("progress must be logical")}

  # Construct and enquote field names
  model <- names(hsi_model)
  hsi   <- paste0("hsi_", model)
  cnt   <- paste0("count_", model)
  area  <- paste0("acres_", model)
  hu    <- paste0("hu_", model)
  model_name <- enquo(model)
  hsi_field  <- enquo(hsi)
  cnt_field  <- enquo(cnt)
  area_field <- enquo(area)
  hu_field   <- enquo(hu)

  # Summarize model for each poly
  sum_df <- exactextractr::exact_extract(hsi_model, polys,
                                         fun = c("mean", "count"),
                                         progress = progress)

  # Calculate cell area in square meters
  terra_rast     <- terra::rast(hsi_model)
  linear_units_m <- terra::linearUnits(terra_rast)
  x_cell_size_m  <- raster::xres(hsi_model) * linear_units_m
  y_cell_size_m  <- raster::yres(hsi_model) * linear_units_m
  cell_area_m2   <- x_cell_size_m * y_cell_size_m

  # 1 sq m = 0.000247105 acres
  acres_sqm <- 0.000247105

  # Rename fields and calculate: area in acres, hu
  sum_df <- sum_df %>%
    rename(!!hsi_field := .data$mean) %>%
    # Replace hsi NaN (produced by mean when cell count is zero)
    mutate(!!hsi_field := base::ifelse(is.nan(!!sym(hsi)), 0, !!sym(hsi))) %>%
    rename(!!cnt_field := .data$count) %>%
    mutate(!!area_field := (!!sym(cnt) * cell_area_m2) * acres_sqm) %>%
    mutate(!!hu_field := !!sym(hsi) * !!sym(area)) %>%
    mutate(ID = as.numeric(row.names(.))) %>%
    relocate(ID, .before = 1)

  return(sum_df)
}
