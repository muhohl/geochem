#' Convert element wt% columns to oxide wt% 
#' 
#' Detects major-elements in `data`, applies the stoichiometric
#' oxide-to-element mass ratio, and returns a tibble of oxide weight percent
#' values. Column name matiching is case-insensitive and tolerates common 
#' alteratnative spellings. 
#' 
#' Recognized elements and their output column names:
#' \describe{
#'   \item{`Na`}{-> `na2o_pct`}
#'   \item{`Mg`}{-> `mgo_pct`}
#'   \item{`Al`}{-> `al2o3_pct`}
#'   \item{`Si`}{-> `sio2_pct`}
#'   \item{`P`}{-> `p2o5_pct`}
#'   \item{`K`}{-> `k2o_pct`}
#'   \item{`Ti`}{-> `tio2_pct`}
#'   \item{`Cr`}{-> `cr2o3_pct`}
#'   \item{`Mn`}{-> `mno_pct`}
#'   \item{`Fe`}{-> `fe2o3_pct` (all iron treated as Fe³⁺)}
#' }
#' 
#' All other columns are dropped. If multiple sources columns resolve to the
#' same element (e.g. duplicate element columns), their values are summed
#' row-wise.
#' 
#' @param data A data frame containing major element columns (wt %).
#' 
#' @return A tibble with one column per recognized element, named with the
#' `_pct` suffix (e.g. `na2o_pct`). Non-oxide columns are not carried forward;
#' use [dplyr::bind_cols()] to reattach identifiers.
#' @export
#' 
#' @import magrittr

elements_to_oxides <- function(data) {

  # Define element masses
  o <- 15.999
  na <- 22.989
  mg <- 24.305
  al <- 26.981
  si <- 28.085
  p <- 30.974
  k <- 39.098
  ti <- 47.867
  cr <- 51.996
  mn <- 54.938
  fe <- 55.845

 # Define oxide masses
  na2o <- 2 * na + o
  mgo <- mg + o
  al2o3 <- 2 * al + 3 * o
  sio2 <- si + 2 * o
  p2o5 <- 2 * p + 5 * o
  k2o <- 2 * k + o
  tio2 <- ti + 2 * o
  cr2o3 <- 2 * cr + 3 * o
  mno <- mn + o
  fe2o3 <- 2 * fe + 3 * o

  # element to oxide ratio
  na_x <- na / na2o
  mg_x <- mg / mgo
  al_x <- al / al2o3
  si_x <- si / sio2
  p_x <- p / p2o5
  k_x <- k / k2o
  ti_x <- ti / tio2
  cr_x <- cr / cr2o3
  mn_x <- mn / mno
  fe_x <- fe / fe2o3

  result <- data |>
    dplyr::rename_with(tolower) |>
    dplyr::select(dplyr::contains(
      c(
        "na_pct",
        "mg_pct",
        "mg_pct",
        "al_pct",
        "si_pct",
        "p_pct",
        "k_pct",
        "ti_pct",
        "cr_pct",
        "mn_pct",
        "mn_pct",
        "fe_pct"
      )
    )) %>%
    {

      oxide_cols <- names(.)

      element_list <- lapply(oxide_cols, function(col) {
        val <- .[[col]]
        # Determine which element this oxide belongs to
        if (stringr::str_detect(col, "na")) {
          out <- val / na_x
        } else if (stringr::str_detect(col, "mg")) {
          out <- val / mg_x
        } else if (stringr::str_detect(col, "al")) {
          out <- val / al_x
        } else if (stringr::str_detect(col, "si")) {
          out <- val / si_x
        } else if (stringr::str_detect(col, "p")) {
          out <- val / p_x
        } else if (stringr::str_detect(col, "k")) {
          out <- val / k_x
        } else if (stringr::str_detect(col, "ti")) {
          out <- val / ti_x
        } else if (stringr::str_detect(col, "cr")) {
          out <- val / cr_x
        } else if (stringr::str_detect(col, "mn")) {
          out <- val / mn_x
        } else if (stringr::str_detect(col, "fe")) {
          out <- val / fe_x
        } else {
          out <- NA_real_
        }

        # Create consistent element column name
        name <- dplyr::case_when(
          stringr::str_detect(col, "na") ~ "na2o_pct",
          stringr::str_detect(col, "mg") ~ "mgo_pct",
          stringr::str_detect(col, "al") ~ "al2o3_pct",
          stringr::str_detect(col, "si") ~ "sio2_pct",
          stringr::str_detect(col, "p_pct") ~ "p2o5_pct",
          stringr::str_detect(col, "k_pct") ~ "k2o_pct",
          stringr::str_detect(col, "ti") ~ "tio2_pct",
          stringr::str_detect(col, "cr") ~ "cr2o3_pct",
          stringr::str_detect(col, "mn") ~ "mno_pct",
          stringr::str_detect(col, "fe") ~ "fe2o3_pct",
          TRUE ~ col
        )

        tibble::tibble(!!name := out)
      })

      # Bind all new element columns together and merge duplicates
      element_df <- dplyr::bind_cols(element_list)

      # If there are duplicate columns (e.g. multiple mg sources), combine them
      element_df <- element_df %>%
        dplyr::reframe(dplyr::across(
          .cols = dplyr::everything(),
          .fns = ~ rowSums(cbind(.x), na.rm = TRUE),
          .names = "{.col}"
        ))

      element_df
    }

  return(result)

}
