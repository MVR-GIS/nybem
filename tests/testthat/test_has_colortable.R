# Create categorical raster
cat <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(1:4, 4))
cat <- raster::ratify(cat)
cat_rat <- levels(cat)[[1]]
cat_rat$label <- c("a", "b", "c", "d")
levels(cat) <- cat_rat
cat@legend@names <- c("a", "b", "c", "d")
cat@legend@colortable <- c("red", "green", "blue", "black")

# Create continuous raster
cont <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(1:4, 4))

test_that("check for colortable", {
  expect_true(has_colortable(cat))
  expect_false(has_colortable(cont))
})

test_that("check errors", {
  expect_error(has_colortable(1))
  expect_error(has_colortable("a"))
})
