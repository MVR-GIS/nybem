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
#'   * hu_<model_name> - HSI score mean for the polygon. Model name is derived
#'                       from the raster data names attribute via names().
#'   * count_<model_name> - Count of cells for the polygon.
#'   * acres_<model_name> - Area in acres for the polygon.
#'
#' @importFrom rlang enquo !! := sym
#' @importFrom exactextractr exact_extract
#' @importFrom raster xres yres
#' @importFrom terra rast cellSize global
#' @importFrom dplyr rename mutate relocate
#' @importFrom magrittr %>%
#' @importFrom sf st_crs
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

  # Calculate cell area in square meters
  # Area method 1: Cheap and fast
  terra_rast     <- terra::rast(hsi_model)
  linear_units_m <- terra::linearUnits(terra_rast)
  x_cell_size_m  <- raster::xres(hsi_model) * linear_units_m
  y_cell_size_m  <- raster::yres(hsi_model) * linear_units_m
  cell_area_m2   <- x_cell_size_m * y_cell_size_m

  # Area method 2: 4 cell corners (more accurate, too complex, too slow)
  # terra_rast     <- terra::rast(hsi_model)
  # cell_area      <- terra::cellSize(terra_rast, unit = "m", transform = TRUE)
  # cell_area_mean <- terra::global(cell_area, fun = "mean", na.rm = TRUE)
  # cell_area_m2   <- cell_area_mean$mean

  # Area method 3: cell mean width * height (approximation at high latitudes)
  # crs_meter    <- sp::CRS(SRS_string = "ESRI:102008") #Albers Equal Area Conic
  # hsi_model_m  <- raster::projectRaster(hsi_model, crs = crs_meter)
  # cell_area    <- suppressWarnings(raster::area(hsi_model_m, na.rm = TRUE))
  # cell_area_m2 <- raster::cellStats(cell_area, stat = "mean", na.rm = TRUE)

  # 1 sq m = 0.000247105 acres
  acres_sqm <- 0.000247105

  # Rename fields and calculate area in acres
  sum_df <- sum_df %>%
    rename(!!hu_field := mean) %>%
    rename(!!cnt_field := count) %>%
    mutate(!!area_field := (!!sym(cnt) * cell_area_m2) * acres_sqm) %>%
    mutate(ID = as.numeric(row.names(.))) %>%
    relocate(ID, .before = 1)

  # Replace hu NaN (produced by mean when cell count is zero)
  sum_df <- sum_df %>%
    mutate(!!hu_field   := base::ifelse(is.nan(!!sym(hu)), 0, !!sym(hu))) %>%
    mutate(!!area_field := base::ifelse(is.nan(!!sym(area)), 0, !!sym(area)))

  return(sum_df)
}
