library(testthat)

test_that("mgt_class_D_B returns a list of ggplot2 layers", {
  result <- mgt_class_D_B()

  expect_type(result, "list")
  expect_true(length(result) > 0)
})

test_that("mgt_class_D_B can be added to a ggplot without error", {
  p <- ggplot2::ggplot() + mgt_class_D_B(labels = FALSE)
  expect_s3_class(p, "gg")
})
