library(testthat)

test_that("hist_plot runs without error for a basic dataset", {
  df <- data.frame(
    Sample_ID = rep(c("S1", "S2"), each = 30),
    Cu_ppm    = c(rnorm(30, 50, 10), rnorm(30, 80, 15))
  )
  # hist_plot prints directly; just check it does not throw
  expect_no_error(hist_plot(df, x_axis = "Cu_ppm", bins = 10))
})
