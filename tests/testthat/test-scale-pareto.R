library(testthat)
library(ggplot2)

test_that("scale_y_pareto validates sum_abs", {
  expect_error(scale_y_pareto(sum_abs = 0), "positive finite")
  expect_error(scale_y_pareto(sum_abs = c(1, 2)), "positive finite")
  expect_error(scale_y_pareto(sum_abs = NA_real_), "positive finite")
})

test_that("scale_y_pareto adds a secondary axis", {
  sc <- scale_y_pareto(sum_abs = 10)
  expect_s3_class(sc, "ScaleContinuousPosition")
  expect_false(is.null(sc$secondary.axis))
})

test_that("scale_y_pareto default secondary transform scales to axis maximum", {
  sc <- scale_y_pareto()
  transform <- sc$secondary.axis$trans
  expect_equal(transform(c(0, 5, 10)), c(0, 50, 100))
})
