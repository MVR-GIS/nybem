# Define a data frame of the Barred Owl suitability curves
# Allen A.W. 1982. Habitat Suitability Index Models: Barred owl.
# U.S. Fish and Wildlife Service. FWS/OBS 82/10.143.
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

# Create the plot
HSIplotter(barredowl_ecorest)

# set parameters for testing within function
SI <- barredowl


test_that("check plot output for continuous metrics", {
  expect_snapshot_output(HSIplotter(barredowl))
  expect_snapshot_output(HSIplotter(barredowl_ecorest))
  vdiffr::expect_doppelganger("HSI-plot-barredowl-continuous",
                              fig = function() HSIplotter(barredowl))
})

test_that("check errors", {
  expect_error(HSIplotter(HSIplotter(barredowl), xlab = c("1")))
  expect_error(HSIplotter(HSIplotter(barredowl), ylab = c("1")))
})
