# Create test models
model_1   <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(1, 4),
                            xmn = 0, xmx = 2, ymn = 0, ymx = 2,
                            crs = "EPSG:26918")
model_2   <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(2, 4),
                            xmn = 0, xmx = 2, ymn = 0, ymx = 2,
                            crs = "EPSG:26918")

# Set the spatial extent of the model summaries
extent <- raster::extent(model_1)

# Create model list
model_list <- list(model_1 = model_1,
                   model_2 = model_2)

# Name the HSI models
hsi_models <- map(names(model_list), ~ name_model(model_list[[.x]], .x))
names(hsi_models) <- names(model_list)

# Create the summaries
summaries <- model_summary(hsi_models, extent)


test_that("check for map output", {
  expect_equal(length(summaries), 2)
})
