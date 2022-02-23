ras <- raster::raster(ncol = 2, nrow = 2)

test_that("check outputs", {
  expect_true(is_RasterLayer(ras))
  expect_false(is_RasterLayer(as.numeric()))
  expect_false(is_RasterLayer(as.character()))
})
