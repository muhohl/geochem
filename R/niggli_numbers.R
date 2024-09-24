
#' Niggli Numbers
#'
#' @param df_chemistry
#'
#' @return
#' @export
#'
#' @examples
niggli_numbers <- function(df_chemistry) {

  df_mol <- geochem::molar_mass()

  df_chem_selected <- df_chemistry |>
    dplyr::rename_with(.fn = tolower, .cols=dplyr::everything()) |>
    dplyr::select(dplyr::any_of(stringr::str_remove(colnames(df_mol), 'mol_')))

  df_chem_mol <- mapply('/', df_chem_selected, df_mol) |>
    tibble::as_tibble()

  df_niggli <- df_chem_mol |>
    dplyr::mutate(
           al =    al2o3+v2o5,
           c =     cao+sro+bao,
           alk =   na2o+k2o+li2o+cs2o+rb2o,
           fm =    (2*fe2o3)+feo+mgo+mno+(2*cr2o3),
           si =    sio2,
           ti =    tio2,
           p =     p2o5,
           h_h2o = h2o,
           n_co2 = co2,
           f =     f2,
           cl =    cl2,
           n_so2 = so2,
           n_s =   s) |>
    dplyr::mutate(total = al+c+alk+fm) |>
    dplyr::mutate(
           al =     (al/total)*100,
           c =      (c/total)*100,
           alk =    (alk/total)*100,
           fm =     (fm/total)*100,
           si =     (si/total)*100,
           ti =     (ti/total)*100,
           p =      (p/total)*100,
           h_h2o =  (h_h2o/total)*100,
           n_co2 =  (n_co2/total)*100,
           f =      (f/total)*100,
           cl =     (cl/total)*100,
           n_so2 =  (n_so2/total)*100,
           n_s =    (s/total)*100) |>
    dplyr::mutate(prim_total = al+c+alk+fm) |>
    dplyr::mutate(
           k = k2o/alk,
           mg = mgo/fm,
           `al-alk` = al-alk,
           `al/alk` = al/alk) |>
    dplyr::mutate(
           qz = si-((6*alk)+(2*`al-alk`)+(c-`al-alk`+fm)),
           `qz_al/alk<1` = si-(100+(3*al)+alk),
           `(al+fm)-(c+alk)` = (al+fm)-(c+alk)) |>
    dplyr::select(-total) |>
    dplyr::select((dplyr::last_col(20)):dplyr::last_col()) |>
    dplyr::bind_cols(df_chemistry |> dplyr::select(1)) |>
    dplyr::select(dplyr::last_col(),1:dplyr::last_col(1))

  return(df_niggli)

}
