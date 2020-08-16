context("Compute empirical Bayesian MNL scores")
library(bwsTools)

test_that("wide argument does what it is supposed to do", {
  res1 <- e_bayescoring(indiv, "id", "block", "label", "value")
  res2 <- e_bayescoring(indiv, "id", "block", "label", "value", wide = TRUE)
  
  expect_equal(ncol(res1), 3)
  expect_equal(ncol(res2), length(unique(indiv$label)) + 1)
})

test_that("E and alpha do what they're supposed to do", {
  res1 <- e_bayescoring(indiv, "id", "block", "label", "value")
  res2 <- e_bayescoring(indiv, "id", "block", "label", "value", E = .5)
  
  expect_true(round(sum(res1$b_ebayes - res2$b_ebayes), 2) != 0)
  
  res3 <- e_bayescoring(indiv, "id", "block", "label", "value")
  res4 <- e_bayescoring(indiv, "id", "block", "label", "value", alpha = 10)
  
  expect_true(var(res3$b_ebayes) > var(res4$b_ebayes))
})

test_that("computation is correct", {
  res <- e_bayescoring(indiv, "id", "block", "label", "value")
  
  expect_equal(
    round(res[res$id == 10 & res$label == "crime", "b_ebayes", drop = TRUE], 2),
    -.46
  )
  expect_equal(
    round(res[res$id == 1 & res$label == "guns", "b_ebayes", drop = TRUE], 2), 
    .2
  )
})

test_that("the result is not a grouped df", {
  expect_false(
    dplyr::is_grouped_df(e_bayescoring(indiv, "id", "block", "label", "value"))
  )
})
