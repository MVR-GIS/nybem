# Get HSI models
hsi_metadata <- nybem::hsi_metadata[1:30, ]
num_of_models <- length(hsi_metadata$model)

# Identify HSI variables required for evaluation
HSI_var_names <- colnames(hsi_metadata[,9:54])
num_HSI_vars <- length(HSI_var_names)

# Create a list of single numeric variables to evaluate HSI equations against
SIV_single_numeric <- as.list(rep_len(c(1), num_HSI_vars))
names(SIV_single_numeric) <- HSI_var_names


test_that("check if model Eqtn can handle single numeric", {
  for(current_model_num in 1:num_of_models) {
    # Subset HSImetadata for current model
    model <- hsi_metadata[current_model_num, ]

    # Calculate HSI
    hsi_result <- eval(parse(text = paste(model$Eqtn)), SIV_single_numeric)

    # print(paste("Model: ",current_model_num))

    expect_true(is.numeric(hsi_result))
  }
})
