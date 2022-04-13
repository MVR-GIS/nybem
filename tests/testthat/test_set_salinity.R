# Create input salinity rasters
sal_1  <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(1, 4))
sal_2  <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(2, 4))
sal_3  <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(3, 4))

marine    <- set_salinity_zone(sal_3, 2, 1)
estuarine <- set_salinity_zone(sal_2, 3, 1)
fresh     <- set_salinity_zone(sal_1, 3, 2)

test_that("check inputs", {
  expect_equal(values(marine), c(1, 1, 1, 1))
  expect_equal(values(estuarine), c(2, 2, 2, 2))
  expect_equal(values(fresh), c(3, 3, 3, 3))
})

test_that("check errors", {
  expect_error(set_salinity_zone(3, 2, 1))
  expect_error(set_salinity_zone(sal_3, "a", 1))
  expect_error(set_salinity_zone(sal_3, 2, "b"))
})
