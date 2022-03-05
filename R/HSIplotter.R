#' @title Plots habitat suitability index curves
#'
#' @description Plots habitat suitability index curves for all metrics in
#'   the model.
#'
#' @export
#' @param SI            matrix; matrix of suitability curves ordered as
#'                      parameter breakpoints and associated suitability
#'                      indices for each parameter with appropriate column
#'                      names.
#'
#' @return A multipanel figure displaying suitability curves.
#'
#' @references
#' * US Fish and Wildlife Service. (1980). Habitat as a basis for environmental
#'   assessment. Ecological Services Manual, 101.
#' * US Fish and Wildlife Service. (1980). Habitat Evaluation Procedures (HEP).
#'   Ecological Services Manual, 102.
#' * US Fish and Wildlife Service. (1981). Standards for the Development of
#'   Habitat Suitability Index Models. Ecological Services Manual, 103.
#'
#' @examples
#'
HSIplotter <- function (SI) {
  # Set number of metrics in the SI model (count of breakpoint-SI pairs)
  nSI <- length(colnames(SI))/2

  # Set par environment
  oldpar <- par("mfrow", "mgp", "mar")
  on.exit(par(oldpar))

  # Set new par environment
  par(mfrow = c(ceiling(nSI/3), 3),
      mgp = c(2, 0.5, 0),
      mar = c(3.5, 3.5, 3, 1))

  # Determine which metrics are continuous
  SI_continuous <- c()
  for (i in 1:nSI) {
    current_metric_column <- 2 * i - 1
    SI_continuous[i] <- is.numeric(current_metric_column)
  }

  # Create the plots
  for (i in 1:nSI) {
    # Set current model values
    current_metric_column <- 2 * i - 1
    current_si_column     <- 2 * i
    metric_vector <- SI[, current_metric_column]
    si_vector     <- SI[, current_si_column]
    current_metric_continuous <- is.numeric(metric_vector)

    if (SI_continuous[i] == TRUE) {
      plot(metric_vector, si_vector,
           pch = 19,
           col = "black",
           xlab = colnames(SI)[current_metric_column],
           ylab = "Suitability Index",
           ylim = c(0, 1))
      lines(metric_vector, si_vector,
            lwd = 2,
            col = "black")
      box()
    }
    if(SI_continuous[i] != TRUE) {
      barplot(si_vector,
              names.arg = metric_vector,
              col = "black",
              xlab = colnames(SI)[current_metric_column],
              ylab = "Suitability Index",
              ylim = c(0, 1))
      box()
    }
  }
}
