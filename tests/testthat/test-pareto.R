library(testthat)
library(ggplot2)

test_that("stat_pareto_bars sorts by absolute value and reports sign", {
  df <- data.frame(
    category = c("a", "b", "c"),
    value = c(2, -5, 3)
  )

  p <- ggplot(df, aes(x = category, y = value)) +
    stat_pareto_bars()

  layer <- ggplot_build(p)$data[[1]]

  expect_equal(layer$x, c(1, 2, 3))
  expect_equal(layer$x_label, c("2", "3", "1"))
  expect_equal(layer$y, c(5, 3, 2))
  expect_equal(layer$sign, c("negative", "positive", "positive"))
})

test_that("stat_pareto_line computes raw and proportional cumulative sums", {
  df <- data.frame(
    category = c("a", "b", "c"),
    value = c(2, -5, 3)
  )

  p_raw <- ggplot(df, aes(x = category, y = value, group = 1)) +
    stat_pareto_line(cumulative = "raw")
  raw_layer <- ggplot_build(p_raw)$data[[1]]

  expect_equal(raw_layer$x, c(1, 2, 3))
  expect_equal(raw_layer$x_label, c("2", "3", "1"))
  expect_equal(raw_layer$y, c(5, 8, 10))

  p_prop <- ggplot(df, aes(x = category, y = value, group = 1)) +
    stat_pareto_line(cumulative = "proportion")
  prop_layer <- ggplot_build(p_prop)$data[[1]]

  expect_equal(prop_layer$y, c(0.5, 0.8, 1.0), tolerance = 1e-12)
})

test_that("geom_pareto_point uses pareto cumulative stat", {
  df <- data.frame(
    category = c("a", "b", "c"),
    value = c(2, -5, 3)
  )

  p <- ggplot(df, aes(x = category, y = value, group = 1)) +
    geom_pareto_point(cumulative = "raw")
  layer <- ggplot_build(p)$data[[1]]

  expect_equal(layer$x, c(1, 2, 3))
  expect_equal(layer$y, c(5, 8, 10))
})
