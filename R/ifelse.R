#' @title Raster Con Function
#'
#' @description This function is designed to behave similar to the ESRI
#'   Spatial Analyst `Con` function. The purpose of this function is to
#'   provide R with a raster safe `ifelse` function.
#'
#' @export
#' @param condition     raster or numeric; an expression that results in a
#'                      boolean, matching the type of the condition.
#' @param true_value    raster, constant; Input true raster or constant value.
#' @param false_value   raster, constant; Input false raster or constant value.
#'
#' @return the true or false value matching the data type of the condition.
#'
#' @details This function was proposed by
#' [Spacedman](https://gis.stackexchange.com/users/865/spacedman) on
#' gis.stackexchange way back in 2013,
#' [What is the equivalent of arcpy "Con" in QGIS and/or R
#' raster-package?](https://gis.stackexchange.com/questions/69734/)
#'
#' @examples
#'
ifelse <- function(condition, true_value, false_value) {
  return(condition * true_value + (!condition) * false_value)
}
