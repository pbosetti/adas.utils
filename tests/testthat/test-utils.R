test_that("example_url works", {
  expect_equal(examples_url("battery.dat"), "https://paolobosetti.quarto.pub/data/battery.dat")
})
