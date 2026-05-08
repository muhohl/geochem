#' Bin point data onto a regular raster grid
#'
#' Creates an \eqn{n \times n} regular grid spanning the range of `x` and `y`,
#' assigns each input observation to its nearest grid cell, and returns the
#' full grid with input data columns joined on. Grid cells with no observations
#' receive `NA` for all data columns. The result can be passed directly to
#' [ggplot2::geom_raster()] using `x_raster` and `y_raster` as coordinates.
#'
#' @param data A data frame with at least two numeric coordinate columns.
#' @param x Column to use as the x coordinate (tidy-select / unquoted).
#' @param y Column to use as the y coordinate (tidy-select / unquoted).
#' @param n Number of grid cells along each axis (default `100`), producing an
#'   \eqn{n \times n} grid.
#'
#' @return A tibble with \eqn{n^2} rows containing:
#'   \describe{
#'     \item{`x_raster`, `y_raster`}{Grid cell centre coordinates.}
#'     \item{`index_x`, `index_y`}{Integer grid indices (1 to `n`).}
#'     \item{...}{All columns from `data`, `NA` where no observation falls in
#'       the cell.}
#'   }
#' @export
#'
#' @examples
#' d <- data.frame(
#'   x      = runif(200, 0, 100),
#'   y      = runif(200, 0, 100),
#'   Au_ppm = runif(200)
#' )
#' raster_map(d, x, y, n = 20)
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
    )

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
}
