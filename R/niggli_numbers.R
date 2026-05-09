#' Niggli Numbers
#'
#' Converts a geochemical data frame of major element oxides into Niggli
#' Numbers following the "Niggli Modified" scheme described by McDonald (2024).
#' Column name matching is case-insensitive (`K2O`, `k2o`, and `K2o` are all
#' recognised).
#'
#' @section Niggli Modified scheme:
#' Chromium is combined with Fe, Mn and Mg in the `fm` term (not with Al as in
#' the classic scheme). Vanadium is combined with Al. The four primary numbers
#' (`al`, `c`, `alk`, `fm`) always sum to 100.
#'
#' Oxide columns are identified by lowercase name. Any recognised oxide that is
#' absent from `data` is treated as zero so that partial analyses are handled
#' gracefully. All other columns (e.g. sample identifiers) are preserved and
#' prepended to the output.
#'
#' @param data Data frame containing major element oxide columns (wt %) **or**
#'   element weight percent columns, or a mix of both. Recognised oxide names
#'   (case-insensitive): `SiO2`, `TiO2`, `Al2O3`, `Fe2O3`, `FeO`, `MnO`,
#'   `MgO`, `CaO`, `Na2O`, `K2O`, `P2O5`, `Cr2O3`, `SrO`, `BaO`, `Li2O`,
#'   `Cs2O`, `Rb2O`, `V2O5`, `NiO`, `H2O`, `CO2`, `F2`, `Cl2`, `SO2`, `S`.
#'   When an oxide column is absent, the function falls back to the
#'   corresponding element column if present: `Si`, `Ti`, `Al`, `Fe` (treated
#'   as FeO-equivalent), `Mn`, `Mg`, `Ca`, `Na`, `K`, `P`, `Cr`, `Sr`, `Ba`,
#'   `Li`, `Cs`, `Rb`, `V`, `Ni` (all case-insensitive). Oxide columns take
#'   precedence over element columns when both exist.
#'
#' @return A tibble with all non-oxide columns from `data` followed by:
#'   \describe{
#'     \item{`al`, `c`, `alk`, `fm`}{Primary Niggli numbers (sum to 100).}
#'     \item{`si`, `ti`, `p`}{Secondary Niggli numbers (unbounded scale).}
#'     \item{`k`}{K2O / (Na2O + K2O + …) — alkali ratio (0–1).}
#'     \item{`mg`}{MgO / (Fe + Mn + Mg + …) — magnesian ratio (0–1).}
#'     \item{`al-alk`}{al minus alk.}
#'     \item{`al/alk`}{al divided by alk.}
#'     \item{`(al+fm)-(c+alk)`}{Niggli sum used in binary diagrams.}
#'     \item{`qz`}{Silica saturation index for rocks with al/alk > 1:
#'       si − (100 + 4·alk).}
#'     \item{`qz*`}{Silica saturation index for rocks with al/alk < 1:
#'       si − (100 + 3·al + alk).}
#'     \item{`h`, `n_co2`, `f`, `cl`, `n_so2`, `n_s`}{Volatile Niggli numbers
#'       (less commonly used).}
#'   }
#' @export
#'
#' @references McDonald, I. (2024). Reimagining Niggli Numbers for modern data
#'   applications in petrology and exploration geochemistry.
#'   *Chemical Geology*, 650, 121915.
#'
#' @examples
#' # Anorthite (CaAl2Si2O8): expect al ≈ 50, c ≈ 50, si ≈ 100
#' anorthite <- data.frame(SiO2 = 43.2, Al2O3 = 36.7, CaO = 20.1)
#' niggli_numbers(anorthite)
#'
#' # Forsterite (Mg2SiO4): expect fm = 100, mg = 1, si ≈ 50
#' forsterite <- data.frame(SiO2 = 42.7, MgO = 57.3)
#' niggli_numbers(forsterite)
niggli_numbers <- function(data) {
  mol_wts <- c(
    sio2 = 60, tio2 = 80, al2o3 = 102, fe2o3 = 160, feo = 72,
    mno = 71, mgo = 40, cao = 56, na2o = 62, k2o = 94,
    p2o5 = 142, cr2o3 = 152, sro = 93.6, bao = 153.3,
    li2o = 29.88, cs2o = 281.8, rb2o = 186.94, v2o5 = 181.88,
    nio = 74.02, h2o = 18, co2 = 44, f2 = 38, cl2 = 70.9,
    so2 = 64.07, s = 32.07
  )

  # Fallback: element wt% → oxide-equivalent moles when the oxide column is
  # absent. Columns: element name (lowercase), atomic mass, cations per oxide
  # formula unit. Fe is treated as FeO-equivalent (Fe2+).
  elem_fallback <- list(
    sio2  = list(col = "si", am = 28.09, n = 1),
    tio2  = list(col = "ti", am = 47.87, n = 1),
    al2o3 = list(col = "al", am = 26.98, n = 2),
    feo   = list(col = "fe", am = 55.85, n = 1),
    mno   = list(col = "mn", am = 54.94, n = 1),
    mgo   = list(col = "mg", am = 24.31, n = 1),
    cao   = list(col = "ca", am = 40.08, n = 1),
    na2o  = list(col = "na", am = 22.99, n = 2),
    k2o   = list(col = "k",  am = 39.10, n = 2),
    p2o5  = list(col = "p",  am = 30.97, n = 2),
    cr2o3 = list(col = "cr", am = 52.00, n = 2),
    sro   = list(col = "sr", am = 87.62, n = 1),
    bao   = list(col = "ba", am = 137.33, n = 1),
    li2o  = list(col = "li", am = 6.94,  n = 2),
    cs2o  = list(col = "cs", am = 132.91, n = 2),
    rb2o  = list(col = "rb", am = 85.47, n = 2),
    v2o5  = list(col = "v",  am = 50.94, n = 2),
    nio   = list(col = "ni", am = 58.69, n = 1)
  )

  data_lc <- dplyr::rename_with(data, tolower)

  # Compute molar values by name; missing oxides fall back to element columns,
  # then to 0 if neither is present.
  mol <- tibble::as_tibble(
    stats::setNames(
      lapply(names(mol_wts), function(ox) {
        if (ox %in% names(data_lc)) {
          dplyr::coalesce(data_lc[[ox]] / mol_wts[[ox]], 0)
        } else if (!is.null(elem_fallback[[ox]]) &&
                   elem_fallback[[ox]]$col %in% names(data_lc)) {
          fb <- elem_fallback[[ox]]
          dplyr::coalesce(data_lc[[fb$col]] / fb$am / fb$n, 0)
        } else {
          rep(0, nrow(data_lc))
        }
      }),
      names(mol_wts)
    )
  )

  # Niggli Modified: Cr goes to fm (not al); V goes to al
  al_r  <- mol$al2o3 + mol$v2o5
  c_r   <- mol$cao   + mol$sro  + mol$bao
  alk_r <- mol$na2o  + mol$k2o  + mol$li2o + mol$cs2o + mol$rb2o
  fm_r  <- 2 * mol$fe2o3 + mol$feo + mol$mgo + mol$mno +
           2 * mol$cr2o3 + mol$nio
  total <- al_r + c_r + alk_r + fm_r

  niggli <- tibble::tibble(
    al              = al_r  / total * 100,
    c               = c_r   / total * 100,
    alk             = alk_r / total * 100,
    fm              = fm_r  / total * 100,
    si              = mol$sio2 / total * 100,
    ti              = mol$tio2 / total * 100,
    p               = mol$p2o5 / total * 100,
    k               = mol$k2o  / alk_r,
    mg              = mol$mgo  / fm_r,
    `al-alk`        = al - alk,
    `al/alk`        = al_r / alk_r,
    `(al+fm)-(c+alk)` = al + fm - c - alk,
    qz              = si - (100 + 4 * alk),
    `qz*`           = si - (100 + 3 * al + alk),
    h               = mol$h2o / total * 100,
    n_co2           = mol$co2 / total * 100,
    f               = mol$f2  / total * 100,
    cl              = mol$cl2 / total * 100,
    n_so2           = mol$so2 / total * 100,
    n_s             = mol$s   / total * 100
  )

  # Carry forward all non-oxide, non-element columns (e.g. sample IDs),
  # using original column names from data to preserve case.
  elem_col_names  <- vapply(elem_fallback, `[[`, character(1), "col")
  id_cols_lc      <- setdiff(names(data_lc), union(names(mol_wts), elem_col_names))
  id_cols_orig    <- names(data)[names(data_lc) %in% id_cols_lc]
  if (length(id_cols_orig) > 0) {
    dplyr::bind_cols(data[, id_cols_orig, drop = FALSE], niggli)
  } else {
    niggli
  }
}
