#' Raster Map
#'
#' Computes a square raster with n tiles from two columns.
#'
#' @param data
#' Provide dataframe with x and y coordinates.
#' @param x
#' Column mapped to x coordinates
#' @param y
#' Column mapped to y coordinates
#' @param n
#' Number of data squares the old dataframe is divided into
#'
#' @return data
#' @export
#'
#' @examples


raster_map <- function(data, x, y, n = 100) {
  x_vector <- seq(
    min(data |> dplyr::pull({{ x }})),
    max(data |> dplyr::pull({{ x }})),
    length.out = n
  )
  y_vector <- seq(
    min(data |> dplyr::pull({{ y }})),
    max(data |> dplyr::pull({{ y }})),
    length.out = n
  )

  # indicate each observation with the right coordinate index
  csvt_ref <- data |>
    dplyr::mutate(
        index_x = purrr::map_int({{ x }}, ~ max(which(x_vector <= .x))),
        index_y = purrr::map_int({{ y }}, ~ max(which(y_vector <= .x)))
    ) #|>
  #select(mid_x, mid_y, bi_ppm, cu_pct, index_x, index_y)

  xy_basic <- tibble::tibble(
    b_x = x_vector,
    index_x = seq_along(x_vector),
    b_y = y_vector,
    index_y = seq_along(y_vector)
  )

  xy_nxnygrid <- expand.grid(
    index_x = xy_basic$index_x,
    index_y = xy_basic$index_y
  )
  xy_xygrid <- expand.grid(new_x = xy_basic$b_x, new_y = xy_basic$b_y)

  xy_expanded <- dplyr::bind_cols(xy_nxnygrid, xy_xygrid)

  raster_df <- dplyr::left_join(xy_expanded, csvt_ref) |>
    dplyr::rename("x_raster" = new_x, "y_raster" = new_y)

  return(raster_df)
  print(paste0(x, "renamed to x_raster and ", y, "renamed to y_raster."))
}
