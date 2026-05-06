library(testthat)

test_that("oxides_to_elements returns correct element columns", {
  df <- tibble::tibble(
    SiO2  = 65.0,
    Al2O3 = 17.0,
    Na2O  = 4.0,
    K2O   = 2.5,
    MgO   = 2.0,
    Fe2O3 = 2.0,
    MnO   = 0.10,
    TiO2  = 0.60,
    P2O5  = 0.12,
    CaO   = 4.5      # CaO is not converted; should be ignored
  )

  result <- oxides_to_elements(df)

  expected_cols <- c("na_pct", "mg_pct", "al_pct", "si_pct",
                     "p_pct",  "k_pct",  "ti_pct", "mn_pct", "fe_pct")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("oxides_to_elements element percentages are lower than oxide input", {
  df <- tibble::tibble(SiO2 = 72.0, MgO = 1.5, Al2O3 = 14.0, Na2O = 3.5)

  result <- oxides_to_elements(df)

  # Si content must be less than SiO2 wt%
  expect_lt(result$si_pct, 72.0)
  # Mg content must be less than MgO wt%
  expect_lt(result$mg_pct, 1.5)
})

test_that("oxides_to_elements is case-insensitive", {
  df_upper <- tibble::tibble(SiO2 = 50.0, Na2O = 3.0)
  df_lower <- tibble::tibble(sio2 = 50.0, na2o = 3.0)

  res_upper <- oxides_to_elements(df_upper)
  res_lower <- oxides_to_elements(df_lower)

  expect_equal(res_upper$si_pct, res_lower$si_pct)
  expect_equal(res_upper$na_pct, res_lower$na_pct)
})

test_that("oxides_to_elements produces physically plausible conversions", {
  # Si makes up 28.085 / (28.085 + 2*15.999) = ~46.7% of SiO2
  df     <- tibble::tibble(SiO2 = 100)
  result <- oxides_to_elements(df)
  expect_equal(result$si_pct, 100 * 28.085 / (28.085 + 2 * 15.999),
               tolerance = 0.01)
})
