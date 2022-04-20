#' @title Are Colors?
#'
#' @description Determines if the input is a vector of valid colors according
#' to the `grDevices::col2rgb` function.
#'
#' @export
#' @param color_vector  vector; A vector of color values.
#'
#' @return A logical vector.
#'
#' @details Follows the advice from [SO](https://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation)
#'
#' @importFrom grDevices col2rgb
#'
are_colors <- function(color_vector) {
  sapply(color_vector,
         function(X) {tryCatch(is.matrix(col2rgb(X)),
                               error = function(e) FALSE)
  })
}
