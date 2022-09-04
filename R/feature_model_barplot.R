#' @title Create a Feature By Ecological Model Barplot
#'
#' @description Creates a barplot from a model summary data frame.
#'
#' @export
#' @param model_summary  data.frame; A data frame returned by the
#'                       `summary_table` function.
#' @param habitat_zone   raster; A habitat zone raster returned by the
#'                       `set_habitat_zone` function.
#' @param ylab           character; The plot y-axis label.
#' @param main           character; The plot main label.
#' @param location       character; `legend` location keyword. One of:
#'                       "bottomright", "bottom", "bottomleft", "left",
#'                       "topleft", "top", "topright", "right" or "center".
#'
#' @return A `barplot` object.
#'
#' @importFrom dplyr relocate filter inner_join arrange
#'
feature_model_barplot <- function(model_summary, habitat_zone,
                                  ylab, main, location = "topleft") {
  # Stacked barplots start drawing the first row at the bottom of the stack.
  # Relocate model summary columns to match legend order. See model_data_t.
  model_sum_reorder <- model_summary %>%
    relocate(mar_deep,  .after = 1) %>%
    relocate(mar_sub,   .after = mar_deep) %>%
    relocate(mar_int,   .after = mar_sub) %>%
    relocate(est_sub,   .after = mar_int) %>%
    relocate(est_int,   .after = est_sub) %>%
    relocate(fresh_tid, .after = est_int)

  # Create pretty model label data frame
  model_abbreviations <- data.frame(zone_name = c("Freshwater",
                                                  "Estuarine-Intertidal",
                                                  "Estuarine-Subtidal",
                                                  "Marine-Intertidal",
                                                  "Marine-Subtidal",
                                                  "Marine-Deep"),
                                    short_name = c("fresh.tid",
                                                   "est.int",
                                                   "est.sub",
                                                   "mar.int",
                                                   "mar.sub",
                                                   "mar.deep"),
                                    # Order to display in legend (1 is top)
                                    legend_order = c(1,2,3,4,5,6))

  # Get model colors from habitat_zone raster
  model_color_df <- data.frame(zone_name = habitat_zone@legend@names,
                               color     = habitat_zone@legend@colortable)

  style_df <- model_color_df %>%
    filter(!grepl("NoData|Upland", zone_name)) %>%
    inner_join(y = model_abbreviations, by = "zone_name") %>%
    arrange(legend_order)

  # Extract feature labels
  feature_names <- model_sum_reorder[, 1]
  # Remove feature labels from data
  model_data <- model_sum_reorder[, 2:length(colnames(model_summary))]
  # Transpose data frame to matrix for stacked barplot display
  model_data_t <- t(model_data)

  barplot(model_data_t,                                   # matrix stacks bars
          beside = FALSE,                                 # stacked bars
          names.arg = feature_names,                      # bar labels
          las = 2,                                        # vertical bar labels
          cex.names = 0.8,                                   # bar label cex
          col = arrange(style_df, desc(legend_order))$color, # reverse colors!
          ylab = ylab,
          main = main)
  # Draw horizontal line to assist reading graph
  grid(nx = NA, ny = NULL, lty = 3, col = "grey80")  # ny=NULL defaults to ticks
  box()
  legend(x = location,
         legend = arrange(style_df, legend_order)$short_name,
         fill   = arrange(style_df, legend_order)$color,
         bg = "white")
}
