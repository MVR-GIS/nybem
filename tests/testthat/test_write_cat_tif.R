# Create categorical raster
cat_ras <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(1, 4))
cat_ras <- raster::ratify(cat_ras)

write_cat_tif(cat_ras, tempdir(), "cat_ras1")
write_cat_tif(cat_ras, tempdir(), "cat_ras2.tif")

test_that("check files written", {
  expect_true(file.exists(file.path(tempdir(), "cat_ras1.tif")))
  expect_true(file.exists(file.path(tempdir(), "cat_ras1.tfw")))
  expect_true(file.exists(file.path(tempdir(), "cat_ras1.tif.vat.dbf")))
  expect_true(file.exists(file.path(tempdir(), "cat_ras1.tif.vat.cpg")))
  expect_true(file.exists(file.path(tempdir(), "cat_ras2.tif")))
  expect_true(file.exists(file.path(tempdir(), "cat_ras2.tfw")))
  expect_true(file.exists(file.path(tempdir(), "cat_ras2.tif.vat.dbf")))
  expect_true(file.exists(file.path(tempdir(), "cat_ras2.tif.vat.cpg")))
})

test_that("check errors", {
  expect_error(write_cat_tiff(7, tempdir(), "numeric"))
  expect_error(write_cat_tiff("a", tempdir(), "character"))
  expect_error(write_cat_tiff(cat_ras, paste0(tempdir(), "1234"),
                              "cat_ras1"))
})

