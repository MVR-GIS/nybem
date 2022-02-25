# americancoot observed metric values
obs_veg_pct <- raster::raster(ncol = 3, nrow = 3)
obs_veg_pct <- raster::setValues(obs_veg_pct,
                                 rep_len(20, raster::ncell(obs_veg_pct)))
obs_emerg_open <- raster::raster(ncol = 3, nrow = 3)
obs_emerg_open <- raster::setValues(obs_emerg_open,
                                    rep_len(1, raster::ncell(obs_emerg_open)))
# Categorical metric: wat.regime
obs_wtr_regime <- raster::raster(ncol = 3, nrow = 3)
obs_wtr_regime <- raster::setValues(obs_wtr_regime,
                                    rep_len(1:6, raster::ncell(obs_wtr_regime)))
obs_wtr_regime <- raster::ratify(obs_wtr_regime)
obs_wtr_regime_rat <- levels(obs_wtr_regime)[[1]]
obs_wtr_regime_rat$class <- c("a", "b", "c", "d", "e", "f")
levels(obs_wtr_regime) <- obs_wtr_regime_rat

# Combine into parameter object
ac_test8 <- list(obs_veg_pct, obs_emerg_open, obs_wtr_regime)

# Get americancoot HSI model record from the `ecorest`package
americancoot_ecorest <- ecorest::HSImodels$americancoot

# set parameters for testing within function
SI         <- americancoot_ecorest
input_proj <- ac_test8

# Calculate SI values
ac_si_8 <- SIcalc(americancoot_ecorest, ac_test8)

# Extract si results
si_veg_pct    <- raster::getValues(ac_si_8[[1]])
si_emerg_open <- raster::getValues(ac_si_8[[2]])
si_wtr_regime <- raster::getValues(ac_si_8[[3]])

# Expected results
expected_si_veg_pct    <- rep(0.55, 9)
expected_si_emerg_open <- rep(0.1, 9)
expected_si_wtr_regime <- c(0.3, 0.9, 1.0, 0.5, 0.0, 0.0, 0.3, 0.9, 1.0)

test_that("check Americancoot numeric and categorical raster results", {
  expect_equal(si_veg_pct, expected_si_veg_pct)
  expect_equal(si_emerg_open, expected_si_emerg_open)
  expect_equal(si_wtr_regime, expected_si_wtr_regime)
})
