# Create categorical raster
cat_ras <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(1, 4))
cat_ras <- raster::ratify(cat_ras)

write_cat_tif(cat_ras, tempdir(), "cat_ras")

test_that("check files written", {
  expect_true(file.exists(file.path(tempdir(), "cat_ras.tif")))
  expect_true(file.exists(file.path(tempdir(), "cat_ras.tfw")))
  expect_true(file.exists(file.path(tempdir(), "cat_ras.tif.vat.dbf")))
  expect_true(file.exists(file.path(tempdir(), "cat_ras.tif.vat.cpg")))
})
