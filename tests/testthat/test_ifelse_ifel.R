# constant rasters
r1 <- raster(ncol = 2, nrow = 2, vals = rep_len(1, 4))

# Convert to terra::SpatRaster
r1_terra <- terra::rast(r1)

test_that("check r1_terra and r1 equal", {
  expect_equal(values(r1_terra)[,"layer"], c(1,1,1,1))
  expect_equal(values(r1), c(1,1,1,1))
})

# Confirm terra::ifel and nybem::ifelse the same
t1 <- terra::ifel(r1_terra < 1, 0, 3)
n1 <- nybem::ifelse(r1 < 1, 0, 3)

test_that("check terra::ifel and nybem::ifelse equal", {
  expect_equal(values(t1)[,"layer"], c(3,3,3,3))
  expect_equal(values(n1), c(3,3,3,3))
})

test_that("benchmark ifelse methods", {
  skip_if(TRUE, "ifelse method benchmarks")
  # Compare methods
  microbenchmark::microbenchmark(
    ifelse(r1 <  1, 0, 3, method = "arith"),
    ifelse(r1 <  1, 0, 3, method = "overlay"),
    terra::rast(r1),
    terra::ifel(r1_terra < 1, 0, 3))

  aoe <- microbenchmark::microbenchmark(
    arith   = nybem::ifelse(r1 > 2, 0, 1, method = "arith"),
    overlay = nybem::ifelse(r1 > 2, 0, 1, method = "overlay"),
    elif    = terra::ifel(r1_terra < 1, 0, 3))
  ggplot2::autoplot(aoe)

  # Compare order of operations
  r1_gte_2 <- r1 < 2

  microbenchmark::microbenchmark(
    r1 < 2,
    nybem::ifelse(r1_gte_2, 0, 1, method = "arith"),
    nybem::ifelse(r1 > 2, 0, 1, method = "arith"))

  microbenchmark::microbenchmark(
    r1 < 2,
    nybem::ifelse(r1_gte_2, 0, 1, method = "overlay"),
    nybem::ifelse(r1 > 2, 0, 1, method = "overlay"))

  # arith-methods vs. calc vs. overlay
  microbenchmark::microbenchmark(
    r1 < 2,
    raster::calc(r1, fun=function(x){x < 2}),
    raster::overlay(r1, fun=function(x){x < 2}))
})

