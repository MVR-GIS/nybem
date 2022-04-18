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

map_cat  <- raster_fig(cat, raster::extent(cat))
map_cont <- raster_fig(cont, raster::extent(cont))

test_that("check outputs", {
  expect_true(class(map_cat)[1] == "tmap")
  expect_true(class(map_cont)[1] == "tmap")
})
