# Create in memory raster object
in_memory <- raster::raster(ncol = 2000, nrow = 2000, vals = rep_len(-2, 4e+06))

# Create on disk raster object
on_disk   <- raster(system.file("external/test.grd", package="raster"))

data_rdata      <- file.path(tempdir(), "data.RData")
no_data_rdata   <- file.path(tempdir(), "no_data.RData")
in_memory_rdata <- file.path(tempdir(), "in_memory.RData")
on_disk_rdata   <- file.path(tempdir(), "on_disk.RData")

# Demonstrate problems saving raster with source on disk
save(in_memory, file = data_rdata)
save(on_disk,   file = no_data_rdata)                      # this is the problem

# Saving correctly handled
save_raster_rdata(in_memory, file = in_memory_rdata)
save_raster_rdata(on_disk,   file = on_disk_rdata)

test_that("verify original raster data configuration", {
  expect_true(raster::inMemory(in_memory))
  expect_false(raster::fromDisk(in_memory))
  expect_false(raster::inMemory(on_disk))
  expect_true(raster::fromDisk(on_disk))
})

test_that("verify save raster behavior" , {
  expect_equal(file.size(data_rdata),    47700, tolerance = 0.05)
  expect_equal(file.size(no_data_rdata),  1300, tolerance = 0.05) #should be 17k
})

test_that("verify save_raster_rdata behavior" , {
  expect_equal(file.size(in_memory_rdata), 47700, tolerance = 0.05)
  expect_equal(file.size(on_disk_rdata),   17000, tolerance = 0.05)
})
