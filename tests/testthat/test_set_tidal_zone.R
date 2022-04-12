# Create input rasters
elev_neg2  <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(-2, 4))
elev_neg1  <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(-1, 4))
elev_neg.5 <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(-0.5, 4))
elev_0     <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(0, 4))
elev_1     <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(1, 4))
elev_2     <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(2, 4))


# Variables for testing
bed_elevation <- elev_neg2
MLLW          <- elev_neg.5
MHHW          <- elev_1

zone_1 <- set_tidal_zone(elev_neg2, elev_neg.5, elev_1)
zone_2 <- set_tidal_zone(elev_neg1, elev_neg.5, elev_1)
zone_3 <- set_tidal_zone(elev_0,    elev_neg.5, elev_1)
zone_4 <- set_tidal_zone(elev_2,    elev_neg.5, elev_1)

test_that("check zone values", {
  expect_equal(values(zone_1), c(1, 1, 1, 1))
  expect_equal(values(zone_2), c(2, 2, 2, 2))
  expect_equal(values(zone_3), c(3, 3, 3, 3))
  expect_equal(values(zone_4), c(4, 4, 4, 4))
})
