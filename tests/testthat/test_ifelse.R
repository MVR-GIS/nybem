# constant rasters
r1 <- raster(ncol = 2, nrow = 2, vals = rep_len(1, 4))
r2 <- raster(ncol = 2, nrow = 2, vals = rep_len(2, 4))

# continuous rasters
r1_4 <- raster(ncol = 2, nrow = 2, vals = 1:4)

# NA raster
r_na_1 <- raster(ncol = 2, nrow = 2, vals = c(1, NA, 1, 1))

# Variables for testing
condition   <- r1 < 1
true_value  <- 0
false_value <- 3
method      <- "overlay"

test_that("check output data type", {
  expect_true(is_RasterLayer(ifelse(r1 <  1, 0, 3)))
  expect_true(!is_RasterLayer(ifelse(1 <  1, 0, 3)))
  expect_true(is.numeric(ifelse(1 <  1, 0, 3)))
})

# method = "arith" -------------------------------------------------------------
test_that("check constant raster input", {
  expect_equal(values(ifelse(r1 <  1, 0, 3)), c(3,3,3,3))
  expect_equal(values(ifelse(r1 == 1, 0, 3)), c(0,0,0,0))
  expect_equal(values(ifelse(r2 <  2, 0, 3)), c(3,3,3,3))
  expect_equal(values(ifelse(r2 == 2, 0, 3)), c(0,0,0,0))
})

test_that("check continuous raster input", {
  expect_equal(values(ifelse(r1_4 <  2, 0, 5)), c(0,5,5,5))
  expect_equal(values(ifelse(r1_4 == 2, 0, 5)), c(5,0,5,5))
})

test_that("check raster math operators", {
  expect_equal(values(ifelse(r1 <  1, r1 + 1, r1 + 2)), c(3,3,3,3))
  expect_equal(values(ifelse(r1 == 1, r1 + 1, r1 + 2)), c(2,2,2,2))
  expect_equal(values(ifelse(r1 <  1, r1 * 2, r1 * 3)), c(3,3,3,3))
  expect_equal(values(ifelse(r1 == 1, r1 * 2, r1 * 3)), c(2,2,2,2))
})

test_that("check raster math functions", {
  expect_equal(values(ifelse(r1 <  1, min(r1, r2) + 1, max(r1, r2) + 1)),
               c(3,3,3,3))
})

test_that("check single numeric input", {
  expect_equal(ifelse(1 <  1, 0, 3), 3)
  expect_equal(ifelse(1 == 1, 0, 3), 0)
})

test_that("check NA handling", {
  expect_equal(values(ifelse(r_na_1 == 1, 0, 5)), c(0, NA, 0, 0))
})

# method = "overlay" -----------------------------------------------------------
test_that("check constant raster input", {
  expect_equal(values(ifelse(r1 <  1, 0, 3, method = "overlay")), c(3,3,3,3))
  expect_equal(values(ifelse(r1 == 1, 0, 3, method = "overlay")), c(0,0,0,0))
  expect_equal(values(ifelse(r2 <  2, 0, 3, method = "overlay")), c(3,3,3,3))
  expect_equal(values(ifelse(r2 == 2, 0, 3, method = "overlay")), c(0,0,0,0))
})

test_that("check continuous raster input", {
  expect_equal(values(ifelse(r1_4 <  2, 0, 5, method = "overlay")), c(0,5,5,5))
  expect_equal(values(ifelse(r1_4 == 2, 0, 5, method = "overlay")), c(5,0,5,5))
})

test_that("check raster math operators", {
  expect_equal(values(ifelse(r1 <  1, r1 + 1, r1 + 2, method = "overlay")),
               c(3,3,3,3))
  expect_equal(values(ifelse(r1 == 1, r1 + 1, r1 + 2, method = "overlay")),
               c(2,2,2,2))
  expect_equal(values(ifelse(r1 <  1, r1 * 2, r1 * 3, method = "overlay")),
               c(3,3,3,3))
  expect_equal(values(ifelse(r1 == 1, r1 * 2, r1 * 3, method = "overlay")),
               c(2,2,2,2))
})

test_that("check raster math functions", {
  expect_equal(values(ifelse(r1 <  1, min(r1, r2) + 1, max(r1, r2) + 1
                             , method = "overlay")),
               c(3,3,3,3))
})

test_that("check single numeric input", {
  expect_equal(ifelse(1 <  1, 0, 3, method = "overlay"), 3)
  expect_equal(ifelse(1 == 1, 0, 3, method = "overlay"), 0)
})

test_that("check NA handling", {
  expect_equal(values(ifelse(r_na_1 == 1, 0, 5, method = "overlay")),
               c(0, NA, 0, 0))
})
