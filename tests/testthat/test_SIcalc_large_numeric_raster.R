# Build generic manual model for testing
# Metric 1
m.1     <- c(0, 2, 4, NA)              # parameter breakpoints
m.1.SIV <- c(0.1, 1, 1, NA)            # parameter suitability indices
m1 <- data.frame(m.1, m.1.SIV)

# Metric 2
m.2      <- c(0, 5, 20, NA)             # parameter breakpoints
m.2.SIV  <- c(0, 0, 1, NA)              # parameter suitability indices
m2 <- data.frame(m.2, m.2.SIV)

# Metric 3
m.3      <- c(0, 20, 60, 100)           # parameter breakpoints
m.3.SIV  <- c(0, 0, 1, 1)               # parameter suitability indices
m3 <- data.frame(m.3, m.3.SIV)

test_model <- data.frame(m1, m2, m3)

# assign test observed metric values
test_ncol <- 6167
test_nrow <- 3333
test_ncell <- test_ncol * test_nrow
test_area_crs <- sp::CRS(SRS_string = "EPSG:6347")   # NAD83(2011)/UTM zone 18N
test_area_extent <- raster::extent(500000,600000,4300000,4485000)  # NJ shore
test_resolution <- c(30, 30)                                       # NLCD res

obs_m1 <- raster::raster(ncol = test_ncol, nrow = test_nrow,
                         vals = rep_len(seq(0, 5, 0.1), test_ncell),
                         ext = test_area_extent,
                         crs = test_area_crs,
                         resolution = test_resolution)
obs_m2 <- raster::raster(ncol = test_ncol, nrow = test_nrow,
                         vals = rep_len(seq(0, 30, 1.1), test_ncell),
                         ext = test_area_extent,
                         crs = test_area_crs,
                         resolution = test_resolution)
obs_m3 <- raster::raster(ncol = test_ncol, nrow = test_nrow,
                         vals = rep_len(seq(0, 100, 2), test_ncell),
                         ext = test_area_extent,
                         crs = test_area_crs,
                         resolution = test_resolution)

# Combine into parameter object
obs_test1 <- list(obs_m1, obs_m2, obs_m3)

# Calculate SI values
test_si_1 <- SIcalc(test_model, obs_test1)

test_that("check output SI raster size matches input", {
  expect_equal(ncell(test_si_1[[1]]), ncell(obs_test1[[1]]))
  expect_equal(ncell(test_si_1[[2]]), ncell(obs_test1[[2]]))
  expect_equal(ncell(test_si_1[[3]]), ncell(obs_test1[[3]]))
})

test_that("check output raster SI values", {
  expect_equal(minValue(test_si_1[[1]]), min(na.omit(m.1.SIV)))
  expect_equal(maxValue(test_si_1[[1]]), max(na.omit(m.1.SIV)))
  expect_equal(minValue(test_si_1[[2]]), min(na.omit(m.2.SIV)))
  expect_equal(maxValue(test_si_1[[2]]), max(na.omit(m.2.SIV)))
  expect_equal(minValue(test_si_1[[3]]), min(na.omit(m.3.SIV)))
  expect_equal(maxValue(test_si_1[[3]]), max(na.omit(m.3.SIV)))
})


