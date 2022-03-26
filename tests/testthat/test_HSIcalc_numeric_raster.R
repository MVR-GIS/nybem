# Barredowl observed metric values
obs_tree_num <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(2, 4))
obs_avg_dbh  <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(5, 4))
obs_can_cov  <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(20, 4))

# Combine into parameter object
bo_test7 <- list(obs_tree_num, obs_avg_dbh, obs_can_cov)

# Get barredowl HSI model record from the `ecorest`package
barredowl_ecorest <- ecorest::HSImodels$barredowl

# Calculate SI values
bo_si_7 <- SIcalc(barredowl_ecorest, bo_test7)

# set parameters for testing within function
si_list <- bo_si_7
method  <- "mean"

# Calculate HSI
hsi_out_7 <- HSIcalc(bo_si_7, "mean")

# Convert HSI raster to
hsi_out_7_vals <- getValues(hsi_out_7)

test_that("check hsi values", {
  expect_equal(hsi_out_7_vals, c(0.333, 0.333, 0.333, 0.333), tolerance = 1e-2)
})
