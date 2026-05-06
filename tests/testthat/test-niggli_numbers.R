library(testthat)

# Minimal whole-rock row that includes all oxides consumed by niggli_numbers().
# Zeros are acceptable; molar_mass() handles the full column list.
basalt_row <- tibble::tibble(
  Sample_ID = "TEST-001",
  SiO2  = 49.0, TiO2  = 1.50, Al2O3 = 15.0, Fe2O3 = 3.0,
  FeO   =  7.0, MnO   =  0.18, MgO  =  9.0, CaO   = 10.0,
  Na2O  =  2.8, K2O   =  0.80, P2O5 =  0.20, Cr2O3 =  0.0,
  SrO   =  0.0, BaO   =  0.0,  Li2O =  0.0,  Cs2O  =  0.0,
  Rb2O  =  0.0, V2O5  =  0.0,  NiO  =  0.0,  H2O   =  0.5,
  CO2   =  0.0, F2    =  0.0,  Cl2  =  0.0,  SO2   =  0.0,
  S     =  0.0
)

test_that("niggli_numbers returns a tibble with expected Niggli parameters", {
  result <- niggli_numbers(basalt_row)

  expect_s3_class(result, "tbl_df")

  expected_cols <- c("al", "c", "alk", "fm", "si", "ti", "p", "k", "mg")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("primary Niggli values (al + c + alk + fm) sum to 100", {
  result <- niggli_numbers(basalt_row)

  primary_sum <- result$al + result$c + result$alk + result$fm
  expect_equal(primary_sum, 100, tolerance = 0.01)
})

test_that("niggli_numbers preserves the first input column (sample ID)", {
  result <- niggli_numbers(basalt_row)

  # The function re-attaches the first column of df_chemistry
  expect_true("Sample_ID" %in% names(result))
  expect_equal(result$Sample_ID, "TEST-001")
})

test_that("niggli k and mg values are in [0, 1]", {
  result <- niggli_numbers(basalt_row)

  expect_gte(result$k,  0)
  expect_lte(result$k,  1)
  expect_gte(result$mg, 0)
  expect_lte(result$mg, 1)
})
