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
#'   * hu_<model_name> - HSI score mean for the polygon.
#'   * acres_<model_name> - Area in acres for the polygon.
#'
#' @importFrom rlang enquo !! :=
#' @importFrom raster extract
#' @importFrom dplyr rename mutate
#'
summarize_by_poly <- function(hsi_model, polys) {
  # Check inputs
  if(!is_RasterLayer(hsi_model)) {stop("hsi_model must be a raster")}
  if(!class(polys)[1] == "sf") {stop("polys must be an sf object")}

  # Construct and enquote field names
  model <- names(hsi_model)
  hu    <- paste0("hu_", model)
  area  <- paste0("acres_", model)
  model_name <- enquo(model)
  hu_field   <- enquo(hu)
  area_field <- enquo(area)

  # Summarize model HSI for each poly
  hu_df <- raster::extract(hsi_model, polys,
                           fun = mean,
                           na.rm = TRUE,
                           df = TRUE)
  # Summarize modeled area (ncell) for each poly
  ncell_vec <- lengths(raster::extract(hsi_model, polys))

  # Join HSI and (calculate) Area
  hu_df <- hu_df %>%
    rename(!!hu_field := !!model_name) %>%
                            # 1 cell = 100 sq m; 1 sq m = 0.000247105 acres
    mutate(!!area_field := (ncell_vec * 100) * 0.000247105)

  return(hu_df)
}
