# Barredowl observed metric values
obs_tree_num <- raster::raster(ncol = 2, nrow = 2)
obs_tree_num <- raster::setValues(obs_tree_num,
                                  rep_len(2, raster::ncell(obs_tree_num)))
obs_avg_dbh  <- raster::raster(ncol = 2, nrow = 2)
obs_avg_dbh  <- raster::setValues(obs_avg_dbh,
                                  rep_len(5, raster::ncell(obs_avg_dbh)))
obs_can_cov  <- raster::raster(ncol = 2, nrow = 2)
obs_can_cov  <- raster::setValues(obs_can_cov,
                                  rep_len(20, raster::ncell(obs_avg_dbh)))

# Combine into parameter object
bo_test7 <- list(obs_tree_num, obs_avg_dbh, obs_can_cov)

# Get barredowl HSI model record from the `ecorest`package
barredowl_ecorest <- ecorest::HSImodels$barredowl

# set parameters for testing within function
SI         <- barredowl_ecorest
input_proj <- bo_test7

# Calculate SI values
bo_si_7 <- SIcalc(barredowl_ecorest, bo_test7)

# Extract si results
si_tree_num <- raster::getValues(bo_si_7[[1]])
si_avg_dbh  <- raster::getValues(bo_si_7[[2]])
si_can_cov  <- raster::getValues(bo_si_7[[3]])

# Expected results
expected_si_tree_num <- c(1, 1, 1, 1)
expected_si_avg_dbh  <- c(0, 0, 0, 0)
expected_si_can_cov  <- c(0, 0, 0, 0)

test_that("check barredowl numeric raster results", {
  expect_equal(si_tree_num, expected_si_tree_num)
  expect_equal(si_avg_dbh, expected_si_avg_dbh)
  expect_equal(si_can_cov, expected_si_can_cov)
})
