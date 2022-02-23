#' @title Calculate Suitability Indicies
#'
#' @description SIcalc computes suitability indices given a set of suitability
#'   curves and project-specific inputs. Suitability indices may be computed
#'   based on either linear interpolation (for continuous variables) or a
#'   lookup method (for categorical variables).
#'
#' @export
#' @param SI           data frame or matrix; matrix of suitability curves
#'                     ordered as parameter breakpoints and associated
#'                     suitability indices for each parameter.
#' @param input_proj   list; A list of numeric or categorical vectors of
#'                     application-specific input parameters associated with
#'                     the suitability curve data from SI.
#'
#' @details The SI parameter must contain a set of column pairs. The column
#' pairs should consist of a
#'
#' @return A vector of the suitability index values that match given user
#'   inputs. Values are returned as equal to the extreme of a range if inputs
#'   are outside of model range.
#'
#' @references
#' * US Fish and Wildlife Service. (1980). Habitat as a basis for environmental
#'   assessment. Ecological Services Manual, 101.
#' * US Fish and Wildlife Service. (1980). Habitat Evaluation Procedures (HEP).
#'   Ecological Services Manual, 102.
#' * US Fish and Wildlife Service. (1981). Standards for the Development of
#'   Habitat Suitability Index Models. Ecological Services Manual, 103.
#'
#' @importFrom raster getValues
#'
SIcalc <- function (SI, input_proj) {
  # Check parameters
  # Check structure of model object - TODO
  # Set number of metrics in the SI model (count of breakpoint-SI pairs)
  nSI <- length(colnames(SI))/2
  # input_proj parameter must be list to preserve metric data type
  if(!is.list(input_proj)) {stop("input_proj must be a list")}
  # Number of inputs must match number of model metrics
  if(length(input_proj) != nSI) {
    stop("Number of inputs does not equal required SI model metrics")}

  # Iterate through each pair of model metric and inputs
  SI_out <- list()
  for(i in 1:nSI) {
    # Set current model values
    current_metric_column <- 2 * i - 1
    current_si_column     <- 2 * i
    metric_vector <- SI[, current_metric_column]
    si_vector     <- SI[, current_si_column]
    current_metric_continuous <- is.numeric(metric_vector)

    # Set current input_proj values
    current_input_raster <- is.raster(input_proj[[i]])
    if(current_input_raster) {
      current_input <- raster::getValues(input_proj[[i]])
    } else {
      current_input <- input_proj[[i]]
    }

    # Check if current metric and input are of the same data type
    current_input_continuous <- is.numeric(current_input)
    if(current_input_continuous != current_metric_continuous) {
      stop("Input data types must match data types of SI model metrics")
    }

    # Calculate continuous SI
    if(current_metric_continuous == TRUE) {
      SI_out[i] <- approx(metric_vector, si_vector,
                          xout = current_input,
                          method = "linear",
                          rule = 2,
                          ties = "ordered")$y
    }

    # Calculate categorical SI
    if(current_metric_continuous != TRUE) {
      SI_out[i] <- SI[which(metric_vector == current_input),
                      current_si_column]
    }
  }
  return(SI_out)
}
