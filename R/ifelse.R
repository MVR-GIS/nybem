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
#' @param method        character; Method for calculating the output raster.
#'                      One of: "arith", "overlay"
#'
#' @details The R raster world has long lacked an efficient equivalent to the
#' ESRI `Con` function. Benchmarking is necessary to determine the efficiency
#' of these various methods for conplex expressions on large real-world
#' rasters.
#'
#' @section method = "arith":
#' The functional form of the method = "arith" was proposed by
#' [Spacedman](https://gis.stackexchange.com/users/865/spacedman) on
#' gis.stackexchange way back in 2013,
#' [What is the equivalent of arcpy "Con" in QGIS and/or R
#' raster-package?](https://gis.stackexchange.com/questions/69734/). The
#' implementation of this method uses the `raster` `Arith-methods` using
#' standard arithmetic operators on rasters.
#'
#' @section method = "overlay":
#' The `raster` package documentation suggests that the `overlay` function
#' may be faster for large rasters and complex functions than the
#' `Arith-methods`. Also see:
#' [Raster ifelse conditional in R?](https://stackoverflow.com/questions/56049806/).
#'
#' @return the true or false value matching the data type of the condition.
#'
#' @importFrom raster setValues overlay
#'
ifelse <- function(condition, true_value, false_value,
                   method = "arith") {
  # if condition not a raster, method = "arith" (other methods require raster)
  if(!is_RasterLayer(condition)) {
    method <- "arith"
  }

  con <- function(condition, true_value, false_value) {
      return(condition * true_value + (!condition) * false_value)}

  # raster arithmetic function -------------------------------------------------
  if(method == "arith") {
    out_raster <- con(condition, true_value, false_value)
  }

  # raster::overlay ------------------------------------------------------------
  if(method == "overlay") {
    if(!is_RasterLayer(true_value)) {
      true_value <- raster::setValues(condition,
                                       values = rep(true_value,
                                                    ncell(condition)))}
    if(!is_RasterLayer(false_value)) {
      false_value <- raster::setValues(condition,
                                        values = rep(false_value,
                                                     ncell(condition)))}


    out_raster <- raster::overlay(condition, true_value, false_value,
                                  fun = con)
  }

  return(out_raster)
}
