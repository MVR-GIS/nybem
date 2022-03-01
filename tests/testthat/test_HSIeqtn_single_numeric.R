# Compute patch quality for the Barred Owl model (no components)
# Allen A.W. 1982. Habitat Suitability Index Models: Barred owl.
# FWS/OBS 82/10.143. U.S. Fish and Wildlife Service.
# https://pubs.er.usgs.gov/publication/fwsobs82_10_143.
# Suitability indices relate to density of large trees, mean diameter of
# overstory trees, and percent canopy cover of overstory.

# Barredowl calculated SI values
bo_test1 <- list(1.0 ,1.0, 1.0)
bo_test2 <- list(0.5, 1.0, 1.0)
bo_test3 <- list(0.0 ,1.0, 1.0)
bo_test4 <- list(0.0, NA,  1.0)                           # missing observation
bo_test5 <- list(1.0, 1.0, 1.0 ,1.0)                      # too many parameters

# Compute patch quality for the Juvenile Alewide mode (two components)
# Pardue, G.B. 1983. Habitat Suitability index models: alewife and blueback
# herring. U.S. Dept. Int. Fish Wildl. Serv. FWS/OBS-82/10.58. 22pp.
# Suitability indices relate to zooplankton density, salinity, and water
# temperature.

# Alewife calculated SI values
al_test1 <- list(1.0, 1.0, 1.0)
al_test2 <- list(0.5, 1.0, 1.0)
al_test3 <- list(0.0, 1.0, 1.0)
al_test4 <- list(1.0, NA,  1.0)                           # missing observation
al_test5 <- list(1.0, 1.0, 1.0, 1.0)                      # too many parameters

# Get model information from ecorest
hsi_metadata <- ecorest::HSImetadata

# Set function parameters for testing
HSImodelname <- "alewifeJuv"
SIV          <- al_test1
HSImetadata  <- hsi_metadata

test_that("check barred owl HSI", {
  expect_equal(HSIeqtn("barredowl", bo_test1, hsi_metadata), 1.00)
  expect_equal(HSIeqtn("barredowl", bo_test2, hsi_metadata), 0.707, tolerance = 0.01)
  expect_equal(HSIeqtn("barredowl", bo_test3, hsi_metadata), 0.00)
})

test_that("check alwife HSI", {
  expect_equal(HSIeqtn("alewifeJuv", al_test1, hsi_metadata), 1.00)
  expect_equal(HSIeqtn("alewifeJuv", al_test2, hsi_metadata), 0.50)
  expect_equal(HSIeqtn("alewifeJuv", al_test3, hsi_metadata), 0.00)
})

test_that("check errors", {
  expect_error(HSIeqtn("barredowl", bo_test4, hsi_metadata))
  expect_error(HSIeqtn("barredowl", bo_test5, hsi_metadata))
  expect_error(HSIeqtn("alewifeJuv", al_test4, hsi_metadata))
  expect_error(HSIeqtn("alewifeJuv", al_test5, hsi_metadata))
  expect_error(HSIeqtn(c("barredowl", "alewifeJuv"),
                       bo_test4, hsi_metadata))
  expect_error(HSIeqtn("barredowl", bo_test4, c(1,2,3)))
  expect_error(HSIeqtn("barrrrrredowl", bo_test4, hsi_metadata))
})
