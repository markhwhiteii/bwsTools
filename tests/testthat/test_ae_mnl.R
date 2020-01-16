context("Analytical estimation of MNL for BWS data")
library(bwsTools)

d <- data.frame(
  totals = c(7145, 7144, 7144, 7144, 7145, 7145, 7144, 7146, 8166, 
             7145, 7144, 7144, 7145, 7144, 7145, 7144, 7146),
  bests = c(1733, 968, 5218, 2704, 2307, 692, 1816, 689, 2483, 1422, 
            362, 2589, 4158, 825, 829, 859, 966),
  worsts = c(1324, 2139, 113, 1010, 772, 3986, 1438, 2397, 1041, 1538, 
             4597, 966, 305, 2875, 2256, 2259, 1604)
)

test_that("proper inputs return data.frame of doubles of proper dimensions", {
  res <- ae_mnl(d, "totals", "bests", "worsts")
  expect_true(all(sapply(res, is.double)))
  expect_true(all(dim(res) == c(nrow(d), 5)))
  expect_true("data.frame" %in% class(res))
})

test_that("no data.frame throws error", {
  bad_d <- list(totals = c(10, 10), worsts = c(3, 5), bests = c(5, 4))
  expect_error(ae_mnl(bad_d, "totals", "bests", "worsts"))
})

test_that("incorrect variable names throws error", {
  expect_error(ae_mnl(d, "X", "bests", "worsts"))
  expect_error(ae_mnl(d, "total", "X", "worsts"))
  expect_error(ae_mnl(d, "totalS", "bests", "X"))
  expect_error(ae_mnl(d, "qqq", "y", "X"))
})

rm(d)
