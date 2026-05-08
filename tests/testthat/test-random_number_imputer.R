library(testthat)

test_that("random_number_imputer replaces '<X' strings with numeric values", {
  df <- data.frame(
    ID  = c("A", "B", "C", "D"),
    Au  = c("<0.5", "1.2", "<0.5", "3.4"),
    Ag  = c("5.0", "<2.0", "8.1", "<2.0"),
    stringsAsFactors = FALSE
  )

  result <- random_number_imputer(df, columns = 2:3)

  expect_type(result$Au, "double")
  expect_type(result$Ag, "double")
  expect_true(all(!is.na(result$Au)))
  expect_true(all(!is.na(result$Ag)))

  # Values that were already numeric should be preserved exactly
  expect_equal(result$Au[2], 1.2)
  expect_equal(result$Au[4], 3.4)
  expect_equal(result$Ag[1], 5.0)
  expect_equal(result$Ag[3], 8.1)

  # Imputed values must be within [0, detection_limit)
  expect_true(result$Au[1] >= 0 && result$Au[1] < 0.5)
  expect_true(result$Au[3] >= 0 && result$Au[3] < 0.5)
  expect_true(result$Ag[2] >= 0 && result$Ag[2] < 2.0)
  expect_true(result$Ag[4] >= 0 && result$Ag[4] < 2.0)
})

test_that("random_number_imputer leaves columns without BDL values unchanged", {
  df <- data.frame(
    ID  = "X",
    val = "3.14",
    stringsAsFactors = FALSE
  )
  result <- random_number_imputer(df, columns = 2)
  expect_equal(result$val, 3.14)
})

test_that("random_number_imputer respects a custom split_symbol", {
  df <- data.frame(
    Cu = c("bdl0.1", "0.8", "bdl0.1"),
    stringsAsFactors = FALSE
  )
  result <- random_number_imputer(df, columns = 1, split_symbol = "bdl")
  expect_type(result$Cu, "double")
  expect_true(result$Cu[1] >= 0 && result$Cu[1] < 0.1)
  expect_equal(result$Cu[2], 0.8)
})
