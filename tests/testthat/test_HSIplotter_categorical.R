# Get americancoot HSI model record from the `ecorest`package
americancoot_ecorest <- ecorest::HSImodels$americancoot

# Create the plot
HSIplotter(americancoot_ecorest)

# set parameters for testing within function
SI <- americancoot_ecorest


test_that("check plot output for categorical metrics", {
  expect_snapshot_output(HSIplotter(americancoot_ecorest))
  vdiffr::expect_doppelganger("HSI-plot-americancoot-categorical",
                              fig = function() HSIplotter(americancoot_ecorest))
})
