# Create model rasters
model_0   <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(0, 4),
                            xmn = 0, xmx = 2, ymn = 0, ymx = 2,
                            crs = "EPSG:26918")
model_1   <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(1, 4),
                            xmn = 0, xmx = 2, ymn = 0, ymx = 2,
                            crs = "EPSG:26918")
model_4   <- raster::raster(ncol = 2, nrow = 2, vals = 1:4,
                            xmn = 0, xmx = 2, ymn = 0, ymx = 2,
                            crs = "EPSG:26918")

# Create test polygons
poly_1 <- raster::rasterToPolygons(model_0, dissolve = TRUE)
poly_4 <- raster::rasterToPolygons(model_0)

# Visually verify raster and polygon dimensions
#plot(model_4)
#plot(poly_1, add = TRUE, border = "red")

# Calculate polygon summaries
sum_0 <- summarize_by_poly(model_0, sf::st_as_sf(poly_1))

test_that("check polygon summary", {
  expect_equal(sum_0$hu_layer, 0)
  expect_equal(sum_0$acres_layer, 0.000247105, tolerance = 0.00001)
})

