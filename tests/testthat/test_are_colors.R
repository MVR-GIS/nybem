c1 <- are_colors(c("black", "red", "green"))
c2 <- are_colors(c("#1f78b4", "#a6cee3", "#33a02c", "#b2df8a"))
c3 <- are_colors(c(1, 2, 3))

test_that("check inputs allowed by col2rgb", {
  expect_true(all(c1))
  expect_true(all(c2))
  expect_true(all(c3))
})

test_that("check false", {
  expect_false(are_colors("b"))
  expect_false(are_colors("blak"))
  expect_false(are_colors("#00"))
  expect_false(are_colors("#RRGGBBAA00"))
})
