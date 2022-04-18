# Create categorical raster
cat_ras <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(1, 4))
cat_ras <- raster::ratify(cat_ras)

# Create a continuous raster
cont_ras <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(1, 4))

test_that("check inputs", {
  expect_true(is_cat_raster(cat_ras))
  expect_false(is_cat_raster(cont_ras))
})

test_that("cehck errors", {
  expect_error(is_cat_raster("a"))
  expect_error(is_cat_raster(7))
})
