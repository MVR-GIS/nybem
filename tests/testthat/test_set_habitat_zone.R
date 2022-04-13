# Create input tidal rasters
tid_0  <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(0, 4))  # not valid
tid_1  <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(1, 4))  # Deep
tid_2  <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(2, 4))  # Subtidal
tid_3  <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(3, 4))  # Intertidal
tid_4  <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(4, 4))  # Upland
tid_5  <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(5, 4))  # not valid

# Create input salinity rasters
sal_0  <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(0, 4))  # not valid
sal_1  <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(1, 4))  # Marine
sal_2  <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(2, 4))  # Estuarine
sal_3  <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(3, 4))  # Fresh
sal_4  <- raster::raster(ncol = 2, nrow = 2, vals = rep_len(4, 4))  # not valid

# Valid combinations
h1_tid4_sal1 <- set_habitat_zone(tid_4, sal_1)  # Upland-Marine
h1_tid4_sal2 <- set_habitat_zone(tid_4, sal_2)  # Upland-Estuarine
h1_tid4_sal3 <- set_habitat_zone(tid_4, sal_3)  # Upland-Fresh
h2_tid1_sal1 <- set_habitat_zone(tid_1, sal_1)  # Deep-Marine
h3_tid2_sal1 <- set_habitat_zone(tid_2, sal_1)  # Subtidal-MArine
h4_tid3_sal1 <- set_habitat_zone(tid_3, sal_1)  # Intertidal-Marine
h5_tid1_sal2 <- set_habitat_zone(tid_1, sal_2)  # Deep-Estuarine
h5_tid2_sal2 <- set_habitat_zone(tid_2, sal_2)  # Subtidal-Estuarine
h6_tid3_sal2 <- set_habitat_zone(tid_3, sal_2)  # Intertidal-Estuarine
h7_tid1_sal3 <- set_habitat_zone(tid_1, sal_3)  # Deep-Fresh
h7_tid2_sal3 <- set_habitat_zone(tid_2, sal_3)  # Subtidal-Fresh
h7_tid3_sal3 <- set_habitat_zone(tid_3, sal_3)  # Intertidal-Fresh

# Invalid combinations
h0_tid3_sal4 <- set_habitat_zone(tid_3, sal_4)
h0_tid5_sal1 <- set_habitat_zone(tid_5, sal_1)
h0_tid1_sal0 <- set_habitat_zone(tid_1, sal_0)

test_that("check inputs for valid combinations", {
  expect_equal(values(h1_tid4_sal1), c(1, 1, 1, 1))
  expect_equal(values(h1_tid4_sal2), c(1, 1, 1, 1))
  expect_equal(values(h1_tid4_sal3), c(1, 1, 1, 1))
  expect_equal(values(h2_tid1_sal1), c(2, 2, 2, 2))
  expect_equal(values(h3_tid2_sal1), c(3, 3, 3, 3))
  expect_equal(values(h4_tid3_sal1), c(4, 4, 4, 4))
  expect_equal(values(h5_tid1_sal2), c(5, 5, 5, 5))
  expect_equal(values(h5_tid2_sal2), c(5, 5, 5, 5))
  expect_equal(values(h6_tid3_sal2), c(6, 6, 6, 6))
  expect_equal(values(h7_tid1_sal3), c(7, 7, 7, 7))
  expect_equal(values(h7_tid2_sal3), c(7, 7, 7, 7))
  expect_equal(values(h7_tid3_sal3), c(7, 7, 7, 7))
})

test_that("check inputs for invalid combinations", {
  expect_equal(values(h0_tid3_sal4), c(0, 0, 0, 0))
  expect_equal(values(h0_tid5_sal1), c(0, 0, 0, 0))
  expect_equal(values(h0_tid1_sal0), c(0, 0, 0, 0))
})

