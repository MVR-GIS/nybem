#' @title Calculate Habitat Suitability Index (HSI)
#'
#' @description Calculates a habitat suitability index for the input
#'   suitability indices using the specified method.
#'
#' @export
#' @param si_list   list; A list of suitability indices.
#' @param method    character; The method for calculating the suitability
#'                  index. Must be one of "mean",
#'
#' @return An HSI object matching the data format of the input suitability
#'   indices.
#'
#' @examples
#' # Build manual model
#' # Number of trees > 51cm diameter per 0.4 ha plot
#' tree.num     <- c(0, 2, 4, NA)              # parameter breakpoints
#' tree.num.SIV <- c(0.1, 1, 1, NA)            # parameter suitability indices
#' tree_num <- data.frame(tree.num, tree.num.SIV)
#'
#' # Mean diameter of overstory trees
#' avg.dbh      <- c(0, 5, 20, NA)             # parameter breakpoints
#' avg.dbh.SIV  <- c(0, 0, 1, NA)              # parameter suitability indices
#' tree_diameter <- data.frame(avg.dbh, avg.dbh.SIV)
#'
#' # Percent canopy cover of overstory trees
#' can.cov      <- c(0, 20, 60, 100)           # parameter breakpoints
#' can.cov.SIV  <- c(0, 0, 1, 1)               # parameter suitability indices
#' canopy_cov <- data.frame(can.cov, can.cov.SIV)
#'
#' barredowl <- data.frame(tree_num, tree_diameter, canopy_cov)
#'
#  # Barredowl observed metric values
#' bo_test1 <- list(2, 5, 20)
#'
#' # Barredowl si values
#' bo_si_1 <- nybem::SIcalc(barredowl, bo_test1)
#'
#' # Calculate HSI
#' bo_hsi <- HSIcalc(si_list = bo_si_1)
#'
#' @importFrom raster brick calc
#'
HSIcalc <- function(si_list, method = "mean") {
  # si_list parameter must be list to preserve metric data type
  if(!is.list(si_list)) {stop("si_list must be a list")}
  if(!(method %in% c("mean"))) {
    stop("method must be one of 'mean', ")}

  # Check input for rasters
  raster_input <- any(unlist(lapply(si_list, is_RasterLayer)))

  # Create a raster brick
  if(raster_input) {
    si <- raster::brick(si_list)
  }

  # Convert list to vector for non-rasters
  if(!raster_input) {
    si <- unlist(si_list)
  }

  if(method == "mean") {
    if(!raster_input) {
      hsi_score <- mean(si, na.rm = TRUE)
    }

    if(raster_input) {
      hsi_score <- raster::calc(si, mean)
    }
  }

  return(hsi_score)
}
