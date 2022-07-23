# Create model rasters
## PROJ6+ method: EPSG:26918 = NAD83 / UTM zone 18N linear units meters
crs_1 <- sp::CRS(SRS_string = "EPSG:26918")
## PROJ6+ method: ESRI:102008 = North_America_Albers_Equal_Area_Conic
crs_2 <- sp::CRS(SRS_string = "ESRI:102008")
## Pick a coordinate system with minimal area distortion for area calc testing
## Use North_America_Albers_Equal_Area_Conic instead of UTM

# 1m pixel = 0.000247105 acres
model_0   <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(0, 4),
                            xmn = 0, xmx = 2, ymn = 0, ymx = 2,
                            crs = crs_2)
names(model_0) <- "model_one"
model_1   <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(1, 4),
                            xmn = 0, xmx = 2, ymn = 0, ymx = 2,
                            crs = crs_2)
names(model_1) <- "model_one"
model_4   <- raster::raster(ncol = 2, nrow = 2, vals = 1:4,
                            xmn = 0, xmx = 2, ymn = 0, ymx = 2,
                            crs = crs_2)
names(model_4) <- "model_one"
model_na   <- raster::raster(ncol = 2, nrow = 2, vals = c(2, 2, NA, NA),
                            xmn = 0, xmx = 2, ymn = 0, ymx = 2,
                            crs = crs_2)
names(model_na) <- "model_one"
model_na_2 <- raster::raster(ncol = 2, nrow = 2, vals = c(NA, NA, NA, NA),
                             xmn = 0, xmx = 2, ymn = 0, ymx = 2,
                             crs = crs_2)
names(model_na_2) <- "model_one"
# 2m pixel = 0.000988422 acres
model2_1   <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(1, 4),
                            xmn = 0, xmx = 4, ymn = 0, ymx = 4,
                            crs = crs_2)
names(model2_1) <- "model_one"
model2_4   <- raster::raster(ncol = 2, nrow = 2, vals = 1:4,
                             xmn = 0, xmx = 4, ymn = 0, ymx = 4,
                             crs = crs_2)
names(model2_4) <- "model_one"

# Create test polygons
poly_1 <- raster::rasterToPolygons(model_0, dissolve = TRUE)
poly_4 <- raster::rasterToPolygons(model_0)
poly2  <- raster::rasterToPolygons(model2_1, dissolve = TRUE)

# Visually verify raster and polygon dimensions
raster::plot(model_1)
plot(sf::st_as_sf(poly_4), add = TRUE, col = NA, border = "red")

# Calculate polygon summaries
sum_0   <- summarize_by_poly(model_0,    sf::st_as_sf(poly_1))
sum_0_2 <- summarize_by_poly(model_na_2, sf::st_as_sf(poly_1))
sum_1   <- summarize_by_poly(model_1,    sf::st_as_sf(poly_1))
sum_2   <- summarize_by_poly(model_na,   sf::st_as_sf(poly_1))
sum2_1  <- summarize_by_poly(model2_1,   sf::st_as_sf(poly2))
sum4_1  <- summarize_by_poly(model_4,    sf::st_as_sf(poly_4))

# Variable for testing
hsi_model <- model_4
polys <- sf::st_as_sf(poly_1)
progress <- TRUE

# Test area calculations
# 4x4 raster, m1 pixel: 1m * 1m = 1 sq m/pixel * 4 pixels = 4 sq meters
#   4 sq meters * 0.000247105 acres/sq meter = 0.000988422 acres
# 4x4 raster, m2 pixel: 2m * 2m = 4 sq m/pixel * 4 pixels = 16 sq meters
#   16 sq meters * 0.000247105 acres/sq meter = 0.00395368 acres

test_that("check field names", {
  expect_true("ID" %in% colnames(sum_0))
  expect_true("hu_model_one" %in% colnames(sum_0))
  expect_true("count_model_one" %in% colnames(sum_0))
  expect_true("acres_model_one" %in% colnames(sum_0))
})

test_that("check 1m polygon summary", {
  # 1 polygon: poly_1
  expect_equal(sum_0$ID, 1)
  # 0 + 0 + 0 + 0 = 0 / 4 data cells = 0 hu: model_0
  expect_equal(sum_0$hu_model_one, 0)
  # 4 data cells
  expect_equal(sum_0$count_model_one, 4)
  # 4 data cells * 1 sq m/pixel = 4 sq m * 0.000247105 ac/sqm = 0.000988422 ac
  expect_equal(sum_0$acres_model_one, 0.000988422, tolerance = 0.00001)
  # 1 polygon: poly_1
  expect_equal(sum_1$ID, 1)
  # 1 + 1 + 1 + 1 = 4 / 4 data cells = 1 hu: model_1
  expect_equal(sum_1$hu_model_one, 1)
  # 4 data cells
  expect_equal(sum_1$count_model_one, 4)
  # 4 data cells * 1 sq m/pixel = 4 sq m * 0.000247105 ac/sqm = 0.000988422 ac
  expect_equal(sum_1$acres_model_one, 0.000988422, tolerance = 0.00001)
})

test_that("check 2m polygon summary", {
  # 1 polygon
  expect_equal(sum2_1$ID, 1)
  # 1 + 1 + 1 + 1 = 4 / 4 data cells = 1 hu
  expect_equal(sum2_1$hu_model_one, 1)
  # 4 data cells
  expect_equal(sum2_1$count_model_one, 4)
  # 4 data cells * 4 sq m/pixel = 16 sq m * 0.000247105 ac/sqm = 0.00395368 ac
  expect_equal(sum2_1$acres_model_one, 0.00395368, tolerance = 0.00001)
})

test_that("check na polygon summary", {
  # 1 polygon
  expect_equal(sum_2$ID, 1)
  # 2 + 2 + NA + NA = 4 / 2 data cells = 2 hu
  expect_equal(sum_2$hu_model_one, 2)
  # 2 data cells
  expect_equal(sum_2$count_model_one, 2)
  # 2 data cells * 1 sq m/pixel = 2 sq m * 0.000247105 ac/sqm = 0.00049421 ac
  expect_equal(sum_2$acres_model_one, 0.00049421, tolerance = 0.00001)
})

test_that("check NaN removed when no model values inside polygon", {
  # 1 polygon
  expect_equal(sum_0_2$ID, 1)
  # NA + NA + NA + NA = NA / NA data cells = NaN hu = 0 hu
  expect_equal(sum_0_2$hu_model_one, 0)
  # 0 data cells
  expect_equal(sum_0_2$count_model_one, 0)
  # 0 data cells * 1 sq m/pixel = 0 sq m = 0 acres
  expect_equal(sum_0_2$acres_model_one, 0, tolerance = 0.00001)
})

test_that("check multiple polys", {
  # 4 polygons
  expect_equal(sum4_1$ID, 1:4)
  # 4 polys, each of 1-4
  expect_equal(sum4_1$hu_model_one, 1:4)
  # 4 polys, 1 cell each
  expect_equal(sum4_1$count_model_one, rep_len(1, 4))
  # 4 polys, 1 cell each equals 0.000247105 acres
  expect_equal(sum4_1$acres_model_one, rep_len(0.000247105, 4),
               tolerance = 0.00001)
})
