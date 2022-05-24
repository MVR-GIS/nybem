model1_unnamed <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(0, 4))

model_name <- "Habitat_Model_1"

model1_named <- name_model(model1_unnamed, model_name)


test_that("check model inputs", {
  expect_equal(names(model1_named), model_name)
})

test_that("check errors", {
  expect_error(name_model("a", model_name))
  expect_error(name_model(model1_unnamed, 7))
})
