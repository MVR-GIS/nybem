#' @title Computes Habitat Suitability Index based on Model-Specified Equation
#'
#' @description HSIeqtn computes a habitat suitability index based on
#'   equations specified in U.S. Fish and Wildlife Service habitat
#'   suitability models contained within ecorest via HSImodels and HSImetadata.
#'   Habitat suitability indices represent an overall assessment of habitat
#'   quality from combining individual suitability indices for multiple
#'   independent variables. The function computes an overall habitat
#'   suitability index.
#'
#' @export
#' @param HSImodelname   character; A character string in quotations that
#'                       must match an existing model name in HSImetadata.
#' @param SIV            list; a vector of suitability index values used in
#'                       the model specified in HSImodelname. Must be listed
#'                       in the same order as specified in the model.
#' @param HSImetadata    data frame; A data frame of HSI model metadata within
#'                       the ecorest package.
#'
#' @return A numeric of the habitat suitability index ranging from 0 to 1.
#'
#' @references
#' * US Fish and Wildlife Service. (1980). Habitat as a basis for environmental
#'   assessment. Ecological Services Manual, 101.
#' * US Fish and Wildlife Service. (1980). Habitat Evaluation Procedures (HEP).
#'   Ecological Services Manual, 102.
#' * US Fish and Wildlife Service. (1981). Standards for the Development of
#'   Habitat Suitability Index Models. Ecological Services Manual, 103.
#'
#' @importFrom dplyr select select_if
#'
HSIeqtn <- function (HSImodelname, SIV, HSImetadata) {
  # Check parameters
  if(!is.character(HSImodelname)) {
    stop("HSImodelname must be of type character")}
  if(length(HSImodelname) != 1) {
    stop("HSImodelname must be a character vector with a single item")}
  if(!is.data.frame(HSImetadata)) {
    stop("HSImetadata must be a data frame.")}
  if(!(HSImodelname %in% HSImetadata$model)) {
    stop("HSImodelname not in HSImetadata$model")}

  # Identify model record number
  model_index_num <- which(HSImetadata$model == HSImodelname)

  # Subset HSImetadata for current model
  model <- HSImetadata[model_index_num, ]

  # Generate a vector of the SI variable names specified in the model
  SIV_var_names <- model %>%
    select(9:30) %>%
    select_if(~ !any(is.na(.))) %>%
    names()

  # Generate a vector of the component variables specified in the model
  used_component_names <- model %>%
    select(31:55) %>%
    select_if(~ !any(is.na(.))) %>%
    names()

  # Generate a vector of all components
  all_component_names <- model %>%
    select(31:55) %>%
    names()

  # Check SIV parameter matches number of specified model metrics
  if(length(SIV_var_names) != length(na.omit(SIV))) {
    stop("SIV vector length does not match model equation.")}
  if(any(is.na(SIV))) {
    stop("SIV vector should not contain any missing data.")}

  # Construct the empty HSI list to hold model SI values and components
  model_SIV_component_names <- c(SIV_var_names, all_component_names)
  HSI <- vector("list", length = length(model_SIV_component_names))
  names(HSI) <- model_SIV_component_names

  # Check for raster SIV input
  raster_siv <- any(sapply(SIV, is_RasterLayer))

  # Assign SI variables to HSI list items
  for (i in 1:length(SIV)) {
    HSI[[i]] <- SIV[[i]]
  }

  # Assign model component equation expressions to named HSI list items
  HSI$CF   <- parse(text = paste(model$CF))
  HSI$CC   <- parse(text = paste(model$CC))
  HSI$CCF  <- parse(text = paste(model$CCF))
  HSI$CWF  <- parse(text = paste(model$CWF))
  HSI$CW   <- parse(text = paste(model$CW))
  HSI$CCB  <- parse(text = paste(model$CCB))
  HSI$CCN  <- parse(text = paste(model$CCN))
  HSI$CWQ  <- parse(text = paste(model$CWQ))
  HSI$CR   <- parse(text = paste(model$CR))
  HSI$CCR  <- parse(text = paste(model$CCR))
  HSI$CD   <- parse(text = paste(model$CD))
  HSI$COT  <- parse(text = paste(model$COT))
  HSI$CL   <- parse(text = paste(model$CL))
  HSI$CEL  <- parse(text = paste(model$CEL))
  HSI$CE   <- parse(text = paste(model$CE))
  HSI$CJ   <- parse(text = paste(model$CJ))
  HSI$CFr  <- parse(text = paste(model$CFr))
  HSI$CS   <- parse(text = paste(model$CS))
  HSI$CA   <- parse(text = paste(model$CA))
  HSI$CI   <- parse(text = paste(model$CI))
  HSI$CNI  <- parse(text = paste(model$CNI))
  HSI$CWFC <- parse(text = paste(model$CWFC))
  HSI$CT   <- parse(text = paste(model$CT))
  HSI$CJA  <- parse(text = paste(model$CJA))
  HSI$Eqtn <- parse(text = paste(model$Eqtn))

  # Evaluate model expressions for each component
  HSI_out <- HSI
  j <- length(SIV_var_names)
  HSI_out[[j + 1]]  <- eval(HSI$CF, HSI)
  HSI_out[[j + 2]]  <- eval(HSI$CC, HSI)
  HSI_out[[j + 3]]  <- eval(HSI$CCF, HSI)
  HSI_out[[j + 4]]  <- eval(HSI$CWF, HSI)
  HSI_out[[j + 5]]  <- eval(HSI$CW, HSI)
  HSI_out[[j + 6]]  <- eval(HSI$CCB, HSI)
  HSI_out[[j + 7]]  <- eval(HSI$CCN, HSI)
  HSI_out[[j + 8]]  <- eval(HSI$CWQ, HSI)
  HSI_out[[j + 9]]  <- eval(HSI$CR, HSI)
  HSI_out[[j + 10]] <- eval(HSI$CCR, HSI)
  HSI_out[[j + 11]] <- eval(HSI$CD, HSI)
  HSI_out[[j + 12]] <- eval(HSI$COT, HSI)
  HSI_out[[j + 13]] <- eval(HSI$CL, HSI)
  HSI_out[[j + 14]] <- eval(HSI$CEL, HSI)
  HSI_out[[j + 15]] <- eval(HSI$CE, HSI)
  HSI_out[[j + 16]] <- eval(HSI$CJ, HSI)
  HSI_out[[j + 17]] <- eval(HSI$CFr, HSI)
  HSI_out[[j + 18]] <- eval(HSI$CS, HSI)
  HSI_out[[j + 19]] <- eval(HSI$CA, HSI)
  HSI_out[[j + 20]] <- eval(HSI$CI, HSI)
  HSI_out[[j + 21]] <- eval(HSI$CNI, HSI)
  HSI_out[[j + 22]] <- eval(HSI$CWFC, HSI)
  HSI_out[[j + 23]] <- eval(HSI$CT, HSI)
  HSI_out[[j + 24]] <- eval(HSI$CJA, HSI)

  # Calculate the final model HSI
  HSI_out[[j + 25]] <- eval(HSI$Eqtn, HSI_out)

  # Remove the unspecified model components
  HSI_results <- HSI_out[which(!is.na(HSI_out))]

  return(HSI_results$Eqtn)
}
