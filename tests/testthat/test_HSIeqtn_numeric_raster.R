# Barredowl observed metric values (no components)
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

# AlewifeJuv observed metric values (two components)
obs_zooplankton   <- raster::raster(ncol = 2, nrow = 2)
obs_zooplankton   <- raster::setValues(obs_zooplankton,
                                  rep_len(1, raster::ncell(obs_zooplankton)))
sal_spring_summer <- raster::raster(ncol = 2, nrow = 2)
sal_spring_summer <- raster::setValues(sal_spring_summer,
                                  rep_len(1, raster::ncell(sal_spring_summer)))
wtr_temp_surface  <- raster::raster(ncol = 2, nrow = 2)
wtr_temp_surface  <- raster::setValues(wtr_temp_surface,
                                  rep_len(1, raster::ncell(wtr_temp_surface)))

# Combine into parameter object
al_test1 <- list(obs_zooplankton, sal_spring_summer, wtr_temp_surface)

# Get model information from ecorest
hsi_metadata <- ecorest::HSImetadata

# Set function parameters for testing
HSImodelname <- "barredowl"
SIV          <- bo_test1
HSImetadata  <- hsi_metadata

# Calculate SI values
bo_hsi_1 <- HSIeqtn("barredowl", bo_test1, hsi_metadata)
al_hsi_1 <- HSIeqtn("alewifeJuv", al_test1, hsi_metadata)

# Extract si results
bo_hsi_results_1 <- raster::values(bo_hsi_1)
al_hsi_results_1 <- raster::values(al_hsi_1)


test_that("check barred owl HSI raster", {
  expect_equal(bo_hsi_results_1, c(1, 1, 1, 1))
})

test_that("check alewifeJuv HSI raster", {
  expect_equal(al_hsi_results_1, c(1, 1, 1, 1))
})

test_that("check errors", {
  expect_error(HSIeqtn("barredowl",
                       list(obs_tree_num, obs_avg_dbh, obs_can_cov,
                            obs_can_cov),
                       hsi_metadata))             # too many SI parameters
  expect_error(HSIeqtn("barredowl",
                       list(obs_tree_num, NA, obs_can_cov),
                       hsi_metadata))             # NA SI parameter
  expect_error(HSIeqtn("barredowl",
                       list(obs_tree_num, obs_can_cov),
                       hsi_metadata))             # not enough SI parameters
})
