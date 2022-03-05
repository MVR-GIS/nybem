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

# set parameters for testing within function
SI <- barredowl

# Check output at console
HSIplotter(barredowl)

test_that("another check", {
  expect_snapshot_output(HSIplotter(barredowl))
  vdiffr::expect_doppelganger("HSI-plot-continuous",
                              fig = function() HSIplotter(barredowl))
})

snapshot_review()
