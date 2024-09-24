
#' Molar Mass
#'
#' @return
#' @export
#'
#' @examples
molar_mass <- function() {
  df_mol <- tibble::tibble(
    mol_sio2 = 60,
    mol_tio2 = 80,
    mol_al2o3 = 102,
    mol_fe2o3 = 160,
    mol_feo = 72,
    mol_mno = 71,
    mol_mgo = 40,
    mol_cao = 56,
    mol_na2o = 62,
    mol_k2o = 94,
    mol_p2o5 = 142,
    mol_cr2o3 = 152,
    mol_sro = 93.6,
    mol_bao = 153.3,
    mol_li2o = 29.88,
    mol_cs2o = 281.8,
    mol_rb2o = 186.94,
    mol_v2o5 = 181.88,
    mol_nio = 74.02,
    mol_h2o = 18,
    mol_co2 = 44,
    mol_f2 = 38,
    mol_cl2 = 70.9,
    mol_so2 = 64.07,
    mol_s = 32.07,
    )
  return(df_mol)
}
