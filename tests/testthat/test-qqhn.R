library(testthat)
library(ggplot2)

test_that("stat_qqhn computes half-normal theoretical quantiles and absolute sample", {
  df <- data.frame(value = c(-2, -1, 0, 1, 2))

  p <- ggplot(df, aes(sample = value)) +
    stat_qqhn()

  layer <- ggplot_build(p)$data[[1]]

  expect_equal(layer$y, sort(abs(df$value)))

  expected_x <- stats::qnorm((stats::ppoints(length(df$value)) + 1) / 2)
  expect_equal(layer$x, expected_x)
})

test_that("stat_qqhn_line returns a 2-point reference line", {
  df <- data.frame(value = c(-4, -2, -1, 0, 1, 2, 4))

  p <- ggplot(df, aes(sample = value)) +
    stat_qqhn_line()

  layer <- ggplot_build(p)$data[[1]]

  expect_equal(nrow(layer), 2)
  expect_true(is.finite(layer$x[1]))
  expect_true(is.finite(layer$y[1]))
  expect_gt(diff(layer$x), 0)
  expect_gt(diff(layer$y), 0)
})

test_that("stat_qqhn_line accepts origin-anchored line.p and passes through origin", {
  df <- data.frame(value = c(-4, -2, -1, 0, 1, 2, 4))

  p <- ggplot(df, aes(sample = value)) +
    stat_qqhn_line(line.p = c(0, 0.25))
  layer <- ggplot_build(p)$data[[1]]

  slope <- diff(layer$y) / diff(layer$x)
  intercept <- layer$y[1] - slope * layer$x[1]
  expect_equal(as.numeric(intercept), 0, tolerance = 1e-10)
})

test_that("stat_qqhn_line rejects length-2 line.p when first value is not zero", {
  df <- data.frame(value = c(-4, -2, -1, 0, 1, 2, 4))
  p <- ggplot(df, aes(sample = value)) + stat_qqhn_line(line.p = c(0.1, 0.25))

  expect_error(ggplot_build(p), "first value must be 0")
})

test_that("geom_qqhn_line works when labels aesthetics are present", {
  df <- data.frame(
    value = c(-4, -2, -1, 0, 1, 2, 4),
    lbl = letters[1:7]
  )

  p <- ggplot(df, aes(sample = value, labels = lbl)) +
    geom_qqhn_line()

  layer <- ggplot_build(p)$data[[1]]
  expect_equal(nrow(layer), 2)
})

test_that("geom_qqhn labels largest values and removes their points", {
  df <- data.frame(value = c(1, 4, 2, 5, 3))

  p <- ggplot(df, aes(sample = value)) +
    geom_qqhn(label_n = 2)

  built <- ggplot_build(p)$data
  point_layer <- built[[1]]
  label_layer <- built[[2]]

  expect_equal(nrow(point_layer), 3)
  expect_equal(sort(label_layer$label), c("4", "5"))
})

test_that("geom_qqhn uses `labels` aesthetic for labeled points", {
  df <- data.frame(
    value = c(1, 4, 2, 5, 3),
    lbl = c("a", "b", "c", "d", "e")
  )

  p <- ggplot(df, aes(sample = value, labels = lbl)) +
    geom_qqhn(label_n = 2)

  label_layer <- ggplot_build(p)$data[[2]]
  expect_equal(sort(label_layer$label), c("b", "d"))
})

test_that("geom_qqhn sets default axis labels", {
  df <- data.frame(value = c(1, 2, 3))
  p <- ggplot(df, aes(sample = value)) + geom_qqhn()

  expect_equal(p$labels$x, "Theoretical quantiles")
  expect_equal(p$labels$y, "Sample quantiles for value")
})

test_that("geom_qqhn and geom_qqhn_line support grouped series", {
  df <- data.frame(
    value = c(-2, -1, 0, 1, 2, -3, -2, 0, 2, 3),
    grp = rep(c("g1", "g2"), each = 5)
  )

  p <- ggplot(df, aes(sample = value, colour = grp, group = grp)) +
    geom_qqhn() +
    geom_qqhn_line()

  built <- ggplot_build(p)$data
  point_layer <- built[[1]]
  line_layer <- built[[2]]

  expect_equal(length(unique(point_layer$group)), 2)
  expect_equal(length(unique(line_layer$group)), 2)
})

test_that("stat_qqhn_band returns a valid envelope", {
  df <- data.frame(value = c(-3, -2, -1, 0, 1, 2, 3))
  p <- ggplot(df, aes(sample = value)) + stat_qqhn_band()
  layer <- ggplot_build(p)$data[[1]]

  expect_gt(nrow(layer), 0)
  expect_true(all(layer$ymax >= layer$ymin))
})

test_that("stat_qqhn_band rejects length-2 line.p when first value is not zero", {
  df <- data.frame(value = c(-3, -2, -1, 0, 1, 2, 3))
  p <- ggplot(df, aes(sample = value)) + stat_qqhn_band(line.p = c(0.1, 0.25))

  expect_error(ggplot_build(p), "first value must be 0")
})

test_that("geom_qqhn_band works without explicit group mapping", {
  df <- data.frame(value = c(-3, -2, -1, 0, 1, 2, 3))
  p <- ggplot(df, aes(sample = value)) + geom_qqhn_band()
  layer <- ggplot_build(p)$data[[1]]

  expect_gt(nrow(layer), 0)
  expect_equal(length(unique(layer$group)), 1)
})
