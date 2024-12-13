test_that("chauvenet works (no outlier)", {
  set.seed(0)
  sink(nullfile())
  c <- chauvenet(rnorm(1000))
  sink()
  expect_false(c$reject)
})

test_that("chauvenet works (outlier)", {
  set.seed(0)
  sink(nullfile())
  c <- chauvenet(c(rnorm(999), 4))
  sink()
  expect_true(c$reject)
})

test_that("daniel_plot_qq works", {
  set.seed(0)
  sink(nullfile())
  daniel_plot_qq(lm(Y~A*B*C*D, data=filtration))
  sink()
  expect_true(TRUE)
})

test_that("daniel_plot_hn works", {
  set.seed(0)
  sink(nullfile())
  daniel_plot_hn(lm(Y~A*B*C*D, data=filtration))
  sink()
  expect_true(TRUE)
})

test_that("pareto_chart for lm works", {
  set.seed(0)
  sink(nullfile())
  pareto_chart(lm(Y~A*B*C*D, data=filtration))
  sink()
  expect_true(TRUE)
})

test_that("pareto_chart for tibble works", {
  set.seed(0)
  sink(nullfile())
  tibble(
    val=rnorm(10, sd=5),
    cat=LETTERS[1:length(val)]
  ) %>%
    pareto_chart(labels=cat, values=val)
  sink()
  expect_true(TRUE)
})


test_that("normplot works", {
  set.seed(0)
  sink(nullfile())
  df <- tibble(
    xn = rnorm(100, mean=20, sd=5),
  )

  df %>% normplot(xn)
  sink()
  expect_true(TRUE)
})
