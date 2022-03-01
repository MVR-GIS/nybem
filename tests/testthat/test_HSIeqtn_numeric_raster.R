# Barredowl observed metric values
obs_tree_num <- raster::raster(ncol = 2, nrow = 2)
obs_tree_num <- raster::setValues(obs_tree_num,
                                  rep_len(1, raster::ncell(obs_tree_num)))
obs_avg_dbh  <- raster::raster(ncol = 2, nrow = 2)
obs_avg_dbh  <- raster::setValues(obs_avg_dbh,
                                  rep_len(1, raster::ncell(obs_avg_dbh)))
obs_can_cov  <- raster::raster(ncol = 2, nrow = 2)
obs_can_cov  <- raster::setValues(obs_can_cov,
                                  rep_len(1, raster::ncell(obs_avg_dbh)))

# Combine into parameter object
bo_test1 <- list(obs_tree_num, obs_avg_dbh, obs_can_cov)

# Get model information from ecorest
hsi_metadata <- ecorest::HSImetadata

# Set function parameters for testing
HSImodelname <- "barredowl"
SIV          <- bo_test1
HSImetadata  <- hsi_metadata

# Calculate SI values
bo_hsi_1 <- HSIeqtn("barredowl", bo_test1, hsi_metadata)

# Extract si results
bo_hsi_results_1 <- raster::values(bo_hsi_1)

test_that("check barred owl HSI", {
  expect_equal(bo_hsi_results_1, c(1, 1, 1, 1))
})
