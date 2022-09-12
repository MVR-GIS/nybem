# Create big rasters
test_ncol <- 6167
test_nrow <- 3333
test_ncell <- test_ncol * test_nrow
suppressWarnings({                                   # PROJ4 to PROJ6 warnings
  test_area_crs <- sp::CRS(SRS_string = "EPSG:6347") # NAD83(2011)/UTM zone 18N
})
test_area_extent <- raster::extent(500000,600000,4300000,4485000)  # NJ shore
test_resolution <- c(30, 30)                                       # NLCD res


test_that("banchmark ifelse method on large rasters", {
  skip_if(TRUE, "ifelse method large raster benchmarks")
  obs_m1 <- raster::raster(ncol = test_ncol, nrow = test_nrow,
                         vals = rep_len(seq(0, 5, 0.1), test_ncell),
                         ext = test_area_extent,
                         crs = test_area_crs,
                         resolution = test_resolution)
  obs_m1_terra <- terra::rast(obs_m1)

  large_aoe <- microbenchmark::microbenchmark(
    arith   = nybem::ifelse(obs_m1 > 2, 0, 1, method = "arith"),
    overlay = nybem::ifelse(obs_m1 > 2, 0, 1, method = "overlay"),
    elif    = terra::ifel(obs_m1_terra > 2, 0, 1)
  )
  ggplot2::autoplot(large_aoe)
})
