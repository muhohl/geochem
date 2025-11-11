#' Oxides to Elements
#'
#' Convert oxides found in the whole rock geochemical data sets to elements.
#'
#' @param data
#' Dataframe as tibble
#'
#' @return A tibble containing the elements.
#' @export
#'
#' @examples
#'
#' @import magrittr

oxides_to_elements <- function(data) {

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
  na_x <- 2 * na / na2o
  mg_x <- mg / mgo
  al_x <- 2 * al / al2o3
  si_x <- si / sio2
  p_x <- 2 * p / p2o5
  k_x <- 2 * k / k2o
  ti_x <- ti / tio2
  cr_x <- 2 * cr / cr2o3
  mn_x <- mn / mno
  fe_x <- 2 * fe / fe2o3

  result <- data |>
    dplyr::select(dplyr::contains(
      c(
        "na2o",
        "mgo",
        "mg_o",
        "al2o3",
        "sio2",
        "si_o2",
        "p2o5",
        "k2o",
        "tio2",
        "ti_o2",
        "cr2o3",
        "mno",
        "mn_o",
        "fe2o3"
      )
    )) %>%
    {

      oxide_cols <- names(.)

      element_list <- lapply(oxide_cols, function(col) {
        val <- .[[col]]
        # Determine which element this oxide belongs to
        if (stringr::str_detect(col, "na")) {
          out <- val * na_x
        } else if (stringr::str_detect(col, "mg")) {
          out <- val * mg_x
        } else if (stringr::str_detect(col, "al")) {
          out <- val * al_x
        } else if (stringr::str_detect(col, "si")) {
          out <- val * si_x
        } else if (stringr::str_detect(col, "p2")) {
          out <- val * p_x
        } else if (stringr::str_detect(col, "k")) {
          out <- val * k_x
        } else if (stringr::str_detect(col, "ti")) {
          out <- val * ti_x
        } else if (stringr::str_detect(col, "cr")) {
          out <- val * cr_x
        } else if (stringr::str_detect(col, "mn")) {
          out <- val * mn_x
        } else if (stringr::str_detect(col, "fe")) {
          out <- val * fe_x
        } else {
          out <- NA_real_
        }

        # Create consistent element column name
        name <- dplyr::case_when(
          stringr::str_detect(col, "na") ~ "na_pct",
          stringr::str_detect(col, "mg") ~ "mg_pct",
          stringr::str_detect(col, "al") ~ "al_pct",
          stringr::str_detect(col, "si") ~ "si_pct",
          stringr::str_detect(col, "p2") ~ "p_pct",
          stringr::str_detect(col, "k") ~ "k_pct",
          stringr::str_detect(col, "ti") ~ "ti_pct",
          stringr::str_detect(col, "cr") ~ "cr_pct",
          stringr::str_detect(col, "mn") ~ "mn_pct",
          stringr::str_detect(col, "fe") ~ "fe_pct",
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
