# Create model rasters
## PROJ6+ method: EPSG:26918 = NAD83 / UTM zone 18N linear units meters
crs_1 <- sp::CRS(SRS_string = "EPSG:26918")
## PROJ6+ method: ESRI:102008 = North_America_Albers_Equal_Area_Conic
crs_2 <- sp::CRS(SRS_string = "ESRI:102008")
## Pick a coordinate system with minimal area distortion for area calc testing
## Use North_America_Albers_Equal_Area_Conic instead of UTM

# 1m pixel = 0.000247105 acres
model_0   <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(1, 4),
                            xmn = 0, xmx = 2, ymn = 0, ymx = 2,
                            crs = crs_2)
names(model_0) <- "m_0"
model_1   <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(0.5, 4),
                            xmn = 0, xmx = 2, ymn = 0, ymx = 2,
                            crs = crs_2)
names(model_1) <- "m_1"

areas <- raster::raster(ncol = 2, nrow = 2, vals = 1:4,
                        xmn = 0, xmx = 2, ymn = 0, ymx = 2,
                        crs = crs_2)

# Create test polygons
poly_1 <- sf::st_as_sf(raster::rasterToPolygons(areas))

# Create model list
model_list <- list(m_zero = model_0,
                   m_one = model_1)

# Recode models
model_recode <- recode_models(model_list)

# Create summary
poly_1_summary <- summarize_models(model_list, poly_1)

# Create summary table
poly_1_area <- summary_table(poly_1_summary, "acres_",
                             poly_1, "layer",
                             "title goes here",
                             digits = 2)

poly_1_hsi <- summary_table(poly_1_summary, "hsi_",
                             poly_1, "layer",
                             "title goes here",
                             digits = 2)
