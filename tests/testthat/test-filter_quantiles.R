library(testthat)

make_df <- function() {
  set.seed(1)
  tibble::tibble(
    Sample  = paste0("S", 1:100),
    Cu_ppm  = c(rnorm(95, mean = 50, sd = 10), rep(500, 5)),   # 5 outliers
    Zn_ppm  = c(rnorm(95, mean = 80, sd = 15), rep(800, 5)),
    rock    = rep(c("A", "B"), 50)
  )
}

test_that("filter_quantiles replaces upper outliers with NA", {
  df     <- make_df()
  result <- filter_quantiles(df, .cols = c("Cu_ppm", "Zn_ppm"),
                             quantile_position = 19, upper_or_lower = "upper")

  # The 5 injected extreme values (500 / 800) should now be NA
  expect_true(any(is.na(result$Cu_ppm)))
  expect_true(any(is.na(result$Zn_ppm)))

  # Non-outlier values should remain intact
  expect_false(all(is.na(result$Cu_ppm)))
})

test_that("filter_quantiles preserves non-numeric columns", {
  df     <- make_df()
  result <- filter_quantiles(df, .cols = list(2), quantile_position = 19)

  expect_equal(result$Sample, df$Sample)
  expect_equal(result$rock,   df$rock)
})

test_that("filter_quantiles preserves original column order", {
  df     <- make_df()
  result <- filter_quantiles(df, .cols = list(2, 3), quantile_position = 19)

  expect_equal(names(result), names(df))
})

test_that("filter_quantiles lower tail works", {
  set.seed(2)
  df <- tibble::tibble(
    val = c(rep(-999, 5), rnorm(95, 100, 10))
  )
  result <- filter_quantiles(df, .cols = list(1), quantile_position = 2,
                             upper_or_lower = "lower")

  expect_true(any(is.na(result$val)))
  # Extreme low values should be the ones replaced
  expect_true(all(result$val[!is.na(result$val)] > -900))
})

test_that("filter_quantiles stops for invalid upper_or_lower argument", {
  df <- make_df()
  expect_error(
    filter_quantiles(df, .cols = list(2), upper_or_lower = "middle"),
    regexp = "upper.*lower|lower.*upper",
    ignore.case = TRUE
  )
})
