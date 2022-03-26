# Create test raster data
ras <- raster(ncol = 2, nrow = 2, vals = rep_len(1, 4))

# Get HSI models
hsi_metadata <- ecorest::HSImetadata[1:30, ]
num_of_models <- length(hsi_metadata$model)

# Identify HSI variables required for evaluation
HSI_var_names <- colnames(hsi_metadata[,9:54])
num_HSI_vars <- length(HSI_var_names)

# Create a list of raster HSI variables to evaluate HSI equations against
SIV_raster <- rep_len(c(ras), num_HSI_vars)
names(SIV_raster) <- HSI_var_names


test_that("check if model Eqtn can handle rasters", {
  for(current_model_num in 1:num_of_models) {
    # Subset HSImetadata for current model
    model <- hsi_metadata[current_model_num, ]

    # Calculate HSI
    hsi_result <- eval(parse(text = paste(model$Eqtn)), SIV_raster)

    # print(paste("Model: ",current_model_num))

    expect_true(is_RasterLayer(hsi_result))
  }
})
