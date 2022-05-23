# Create continuous raster
cont_ras <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(1, 4))

write_cont_tiff(cont_ras, tempdir(), "cont_ras1")
write_cont_tiff(cont_ras, tempdir(), "cont_ras2.tif")

test_that("check files written", {
  expect_true(file.exists(file.path(tempdir(), "cont_ras1.tif")))
  expect_true(file.exists(file.path(tempdir(), "cont_ras2.tif")))
})

test_that("check errors", {
  expect_error(write_cont_tiff(7, tempdir(), "numeric"))
  expect_error(write_cont_tiff("a", tempdir(), "character"))
  expect_error(write_cont_tiff(cont_ras, paste0(tempdir(), "1234"),
                               "cont_ras1"))
})
