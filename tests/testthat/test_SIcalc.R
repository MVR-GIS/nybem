# Build and define a matrix of the Barred Owl suitability curves
# Allen A.W. 1982. Habitat Suitability Index Models: Barred owl.
# FWS/OBS 82/10.143.
# U.S. Fish and Wildlife Service.
# https://pubs.er.usgs.gov/publication/fwsobs82_10_143.
#
# Number of trees > 51cm diameter per 0.4 ha plot
tree.num     <- c(0, 2, 4, NA)              # parameter breakpoints
tree.num.SIV <- c(0.1, 1, 1, NA)            # parameter suitability indices
tree_num <- cbind(tree.num, tree.num.SIV)

# Mean diameter of overstory trees
avg.dbh.in   <- c(0, 5, 20, NA)             # parameter breakpoints
avg.dbh.SIV  <- c(0, 0, 1, NA)              # parameter suitability indices
tree_diameter <- cbind(avg.dbh.in, avg.dbh.SIV)

# Percent canopy cover of overstory trees
can.cov      <- c(0, 20, 60, 100)           # parameter breakpoints
can.cov.SIV  <- c(0, 0, 1, 1)               # parameter suitability indices
canopy_cov <- cbind(can.cov, can.cov.SIV)

barredowl_m <- cbind(tree_num, tree_diameter, canopy_cov)

# Get barredowl from `ecorest`
barredowl_ecorest <- ecorest::HSImodels$barredowl

# Barredowl known values
bo_test1 <- c(2, 5, 20)
bo_test2 <- c(4, 20, 60)
bo_test3 <- c(4, 20, 40)
bo_test4 <- c(0, 12.5, 40)
bo_test5 <- c(4, 40, 60)
bo_test6 <- c(4, NA, 60)

test_that("check SI outputs", {
  expect_equal(SIcalc(barredowl_m, bo_test1), c(1.0, 0.0, 0.0))
  expect_equal(SIcalc(barredowl_m, bo_test2), c(1.0, 1.0, 1.0))
  expect_equal(SIcalc(barredowl_m, bo_test3), c(1.0, 1.0, 0.5))
  expect_equal(SIcalc(barredowl_m, bo_test4), c(0.1, 0.5, 0.5))
  expect_equal(SIcalc(barredowl_m, bo_test5), c(1.0, 1.0, 1.0))
  expect_equal(SIcalc(barredowl_m, bo_test6), c(1.0, NA,  1.0))
})

test_that("check SI outputs from ecorest::HSImodels", {
  expect_equal(SIcalc(barredowl_ecorest, bo_test1), c(1.0, 0.0, 0.0))
  expect_equal(SIcalc(barredowl_ecorest, bo_test2), c(1.0, 1.0, 1.0))
  expect_equal(SIcalc(barredowl_ecorest, bo_test3), c(1.0, 1.0, 0.5))
  expect_equal(SIcalc(barredowl_ecorest, bo_test4), c(0.1, 0.5, 0.5))
  expect_equal(SIcalc(barredowl_ecorest, bo_test5), c(1.0, 1.0, 1.0))
  expect_equal(SIcalc(barredowl_ecorest, bo_test6), c(1.0, NA,  1.0))
})
