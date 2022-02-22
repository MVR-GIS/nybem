#' @title Calculate Suitability Indicies
#'
#' @description SIcalc computes suitability indices given a set of suitability
#'   curves and project-specific inputs. Suitability indices may be computed
#'   based on either linear interpolation (for continuous variables) or a
#'   lookup method (for categorical variables).
#'
#' @export
#' @param SI           matrix; matrix of suitability curves ordered as
#'                     parameter breakpoints and associated suitability
#'                     indices for each parameter.
#' @param input.proj   vector; numeric or categorical vector of
#'                     application-specific input parameters associated with
#'                     the suitability curve data from SI.
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
SIcalc <- function (SI, input.proj) {
  nSI <- length(colnames(SI))/2
  SI.cont <- c()
  for (i in 1:nSI) {
    SI.cont[i] <- is.numeric(SI[1, 2 * i - 1])
  }
  SI.out <- c()
  for (i in 1:nSI) {
    if (length(input.proj) != nSI) {
      SI.out <- "Number of inputs does not equal number of SI values."
      break
    }
    else if (is.na(input.proj[i]) | input.proj[i] == "NA") {
      SI.out[i] <- NA
    }
    else if (SI.cont[i] == TRUE) {
      SI.out[i] <- approx(SI[, 2 * i - 1], SI[, i * 2],
                          xout = input.proj[i], method = "linear",
                          rule = 2, ties = "ordered")$y
    }
    else {
      SI.out[i] <- SI[which(SI[, i * 2 - 1] == input.proj[i]),
                      i * 2]
    }
  }
  return(SI.out)
}
