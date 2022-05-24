model_neg_1 <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(-1, 4))
model_0     <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(0,  4))
model_1     <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(1,  4))

model_recode_na <- na_outside_model(model_neg_1)
model_recode_0  <- na_outside_model(model_0)
model_recode_1  <- na_outside_model(model_1)

test_that("check model calcs", {
  expect_equal(values(model_recode_na), as.double(c(NA, NA, NA, NA)))
  expect_equal(values(model_recode_0), c(0, 0, 0, 0))
  expect_equal(values(model_recode_1), c(1, 1, 1, 1))
})
