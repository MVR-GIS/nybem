# Build and define a matrix of the Barred Owl suitability curves
# Allen A.W. 1982. Habitat Suitability Index Models: Barred owl.
# FWS/OBS 82/10.143.
# U.S. Fish and Wildlife Service.
# https://pubs.er.usgs.gov/publication/fwsobs82_10_143.

# Build manual model
# Number of trees > 51cm diameter per 0.4 ha plot
tree.num     <- c(0, 2, 4, NA)              # parameter breakpoints
tree.num.SIV <- c(0.1, 1, 1, NA)            # parameter suitability indices
tree_num <- data.frame(tree.num, tree.num.SIV)

# Mean diameter of overstory trees
avg.dbh      <- c(0, 5, 20, NA)             # parameter breakpoints
avg.dbh.SIV  <- c(0, 0, 1, NA)              # parameter suitability indices
tree_diameter <- data.frame(avg.dbh, avg.dbh.SIV)

# Percent canopy cover of overstory trees
can.cov      <- c(0, 20, 60, 100)           # parameter breakpoints
can.cov.SIV  <- c(0, 0, 1, 1)               # parameter suitability indices
canopy_cov <- data.frame(can.cov, can.cov.SIV)

barredowl <- data.frame(tree_num, tree_diameter, canopy_cov)

# Get barredowl HSI model record from the `ecorest`package
barredowl_ecorest <- ecorest::HSImodels$barredowl

# Barredowl observed metric values
bo_test1 <- list(2, 5, 20)
bo_test2 <- list(4, 20, 60)
bo_test3 <- list(4, 20, 40)
bo_test4 <- list(0, 12.5, 40)
bo_test5 <- list(4, 40, 60)
bo_test6 <- list(4, as.double(NA), 60)

# set parameters for testing within function
SI         <- barredowl_ecorest
input_proj <- bo_test1

test_that("manual model, check SI outputs against known values", {
  expect_equal(SIcalc(barredowl, bo_test1), list(1.0, 0.0, 0.0))
  expect_equal(SIcalc(barredowl, bo_test2), list(1.0, 1.0, 1.0))
  expect_equal(SIcalc(barredowl, bo_test3), list(1.0, 1.0, 0.5))
  expect_equal(SIcalc(barredowl, bo_test4), list(0.1, 0.5, 0.5))
  expect_equal(SIcalc(barredowl, bo_test5), list(1.0, 1.0, 1.0))
  expect_equal(SIcalc(barredowl, bo_test6), list(1.0, as.double(NA),  1.0))
})

test_that("ecorest::HSImodels, check SI outputs against known values", {
  expect_equal(SIcalc(barredowl_ecorest, bo_test1), list(1.0, 0.0, 0.0))
  expect_equal(SIcalc(barredowl_ecorest, bo_test2), list(1.0, 1.0, 1.0))
  expect_equal(SIcalc(barredowl_ecorest, bo_test3), list(1.0, 1.0, 0.5))
  expect_equal(SIcalc(barredowl_ecorest, bo_test4), list(0.1, 0.5, 0.5))
  expect_equal(SIcalc(barredowl_ecorest, bo_test5), list(1.0, 1.0, 1.0))
  expect_equal(SIcalc(barredowl_ecorest, bo_test6), list(1.0, as.double(NA), 1.0))
})

test_that("check errors", {
  expect_error(SIcalc(barredowl_ecorest, list(2, 5)))        # missing input
  expect_error(SIcalc(barredowl_ecorest, list(2, 5, "a")))   # input wrong type
})




