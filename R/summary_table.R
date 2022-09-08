#' @title HSI Model Summary Table.
#'
#' @description Produces an HSI model summary table for the specified metric.
#'
#' @export
#' @param summary_df     data.frame; A model summary data frame created by the
#'                       `summarize_models` function.
#' @param model_metric   character; The model metric to be summarized from the
#'                       `summary_df`. One of "hsi_", "acres_", or "hu_".
#' @param polys          sf; A sf polygon object.
#' @param poly_field     character; The `polys` field used to identify the
#'                       summarization features.
#' @param caption        character; A caption for the table.
#' @param digits         numeric; Number of decimal places to round metric.
#'
#' @return A model summary data frame and a `knitr::kable` table object (is
#' returned silently).
#'
#' @importFrom rlang sym := !! .data
#' @importFrom dplyr mutate select inner_join relocate contains
#' @importFrom purrr set_names
#' @importFrom stringr str_replace_all
#' @importFrom knitr kable
#'
summary_table <- function(summary_df,
                          model_metric = c("hsi_", "acres_", "hu_"),
                          polys, poly_field,
                          caption, digits = 2) {
  # Capture `poly_field` as symbol
  poly_field_name <- rlang::sym(poly_field)

  # Create a polys features df for labeling
  polys_label_df <- polys %>%
    dplyr::mutate(ID = as.numeric(row.names(.))) %>%
    dplyr::mutate(feature_name := !!poly_field_name) %>%
    dplyr::select(ID, .data$feature_name)

  # Extract metric from summary
  summary_df_metric <- summary_df %>%
    dplyr::select(contains(model_metric)) %>%
    purrr::set_names(~ stringr::str_replace_all(., model_metric, "")) %>%
    dplyr::mutate(ID = as.numeric(row.names(.))) %>%
    dplyr::inner_join(polys_label_df, by = "ID") %>%
    dplyr::relocate(feature_name, .before = 1) %>%
    dplyr::select(-c(ID, .data$geometry))

  # Create table
  col_names <- stringr::str_replace_all(colnames(summary_df_metric), "_", " ")

  print(knitr::kable(summary_df_metric,
                     col.names = col_names,
                     digits = digits,
                     caption = caption))

  return(summary_df_metric)
}
