library(testthat)

test_that("geom_magnetite_deposit returns a list of ggplot2 layers", {
  result <- geom_magnetite_deposit()

  expect_type(result, "list")
  expect_true(length(result) > 0)
})

test_that("geom_magnetite_deposit can be added to a ggplot without error", {
  p <- ggplot2::ggplot() + geom_magnetite_deposit(labels = FALSE)
  expect_s3_class(p, "gg")
})
