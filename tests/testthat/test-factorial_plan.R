test_that("fp_defrel works (formula)", {
  dr <- fp_defrel(~A*B*C)
  expect_equal(as.character(dr), as.character(~A*B*C))
})

test_that("fp_defrel works (number)", {
  dr <- fp_defrel(3)
  expect_equal(as.character(dr), as.character(~A*B*C))
})


test_that("fp_treatments works (formula)", {
  expect_equal(
    fp_treatments(~A*B*C),
    c("(1)", "a", "b",  "ab", "c", "ac", "bc", "abc")
  )
})

test_that("fp_treatments works (number)", {
  expect_equal(
    fp_treatments(3),
    c("(1)", "a", "b",  "ab", "c", "ac", "bc", "abc")
  )
})


test_that("fp_design_matrix works (formula)", {
  treat <- fp_design_matrix(~A*B*C) %>%
    pull(.treat)
  expect_equal(treat, c("(1)", "a", "b", "ab", "c", "ac", "bc", "abc"))
})

test_that("fp_augment_* work", {
  df <- fp_design_matrix(2) %>%
    fp_add_names(A="time", B="temperature") %>%
    fp_add_scale(A=c(20, 25), B=c(75, 125), suffix = ".s") %>%
    fp_augment_center(rep=5) %>%
    fp_augment_axial()

  A_s <- df %>% pull(A.s)

  expect_equal(A_s, c(20, 25, 20, 25, 22.5, 22.5, 22.5, 22.5, 22.5, 22.5, 18.9644660940673,
                      26.0355339059327, 22.5))
})

test_that("fp_design_matrix works (number)", {
  treat <- fp_design_matrix(3) %>%
    pull(.treat)
  expect_equal(treat, c("(1)", "a", "b", "ab", "c", "ac", "bc", "abc"))
})

test_that("fp_alias_list works", {
  expect_equal(
    fp_alias_list(~A*B*C),
    list(A = "BC", B = "AC", AB = "C", C = "AB", AC = "B", BC = "A",
           ABC = character(0))
  )
})


test_that("fp_alias_matrix works", {
  expect_true({
    fp_design_matrix(5) %>%
      fp_fraction(~A*B*C*D) %>%
      fp_fraction(~B*C*D*E) %>%
      fp_alias_matrix() %>%
      plot()
    TRUE
  })
})
