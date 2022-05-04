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

# Barredowl observed metric values
bo_test1 <- list(2, 5, 20)
bo_test2 <- list(4, 20, 60)

# Barredowl si values
bo_si_1 <- nybem::SIcalc(barredowl, bo_test1)
bo_si_2 <- nybem::SIcalc(barredowl, bo_test2)

# set parameters for testing within function
si_list <- bo_si_1
method  <- "mean"

test_that("check hsi values", {
  expect_equal(HSIcalc(bo_si_1), 0.333, tolerance = 1e-2)
  expect_equal(HSIcalc(bo_si_2), 1)
})
