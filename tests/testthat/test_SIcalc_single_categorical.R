# Get americancoot HSI model record from the `ecorest`package
americancoot_ecorest <- ecorest::HSImodels$americancoot

# American Coot observed metric values
ac_test1 <- list(20, 1, "a")
ac_test2 <- list(20, 1, "b")
ac_test3 <- list(20, 1, "c")
ac_test4 <- list(20, 1, "d")
ac_test5 <- list(20, 1, "e")
ac_test6 <- list(20, 1, "f")
ac_test7 <- list(20, 1, "g")

# set parameters for testing within function
SI         <- americancoot_ecorest
input_proj <- ac_test1

test_that("ecorest::HSImodels, check outputs", {
  expect_equal(SIcalc(americancoot_ecorest, ac_test1), list(0.55, 0.1, 0.3))
  expect_equal(SIcalc(americancoot_ecorest, ac_test2), list(0.55, 0.1, 0.9))
  expect_equal(SIcalc(americancoot_ecorest, ac_test3), list(0.55, 0.1, 1.0))
  expect_equal(SIcalc(americancoot_ecorest, ac_test4), list(0.55, 0.1, 0.5))
  expect_equal(SIcalc(americancoot_ecorest, ac_test5), list(0.55, 0.1, 0.0))
  expect_equal(SIcalc(americancoot_ecorest, ac_test6), list(0.55, 0.1, 0.0))
  expect_equal(SIcalc(americancoot_ecorest, ac_test7), list(0.55, 0.1,
                                                            as.double(NA)))
})


