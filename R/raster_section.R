#' Raster cross-section for drillhole data
#'
#' Projects drillhole x/y coordinates onto a vertical section plane (using the
#' same rotation as [drillhole_section()]) and bins the resulting 2D positions
#' (projected horizontal distance × depth) into a regular n×n grid. Because
#' dense drillhole datasets often have many overlapping samples when viewed in
#' cross-section, each grid cell is collapsed to a single representative value
#' via `agg_fn`.
#'
#' The horizontal projection follows:
#' \deqn{x_{\text{proj}} = x \cos(\theta) - y \sin(\theta)}
#' where \eqn{\theta} is `view_angle` in degrees.
#'
#' @param data Data frame with drillhole coordinate and assay columns.
#' @param x Name of the easting column (default `"x"`).
#' @param y Name of the northing column (default `"y"`).
#' @param z Name of the depth column (default `"z"`).
#' @param view_angle Viewing azimuth in degrees (default `0`). See
#'   [drillhole_section()] for the cardinal-direction mapping.
#' @param n Number of grid cells along each axis (default `100`), producing an
#'   n×n grid in projected-distance × depth space.
#' @param agg_fn Aggregation function applied to all numeric columns within
#'   each grid cell. Default `mean`. Pass `median` for median aggregation.
#' @param group_col Name of a categorical column (e.g. `"lithology"`) to use
#'   as an additional grouping variable. When supplied the output contains one
#'   row per grid cell × group level. `NULL` (default) aggregates across all
#'   observations in each cell.
#'
#' @return A data frame with columns `proj_x_raster` (binned projected
#'   horizontal distance), `z_raster` (binned depth), `index_x`, `index_z`,
#'   all original columns aggregated by `agg_fn`, and `n` (observation count
#'   per cell).
#' @export
#'
#' @examples
#' dh <- data.frame(
#'   hole_id   = rep(paste0("DH-0", 1:3), each = 20),
#'   mid_x         = rep(c(100, 200, 300), each = 20),
#'   mid_y         = rep(c(100, 150, 120), each = 20),
#'   mid_z         = rep(seq(0, 190, by = 10), 3),
#'   Au_ppm    = runif(60),
#'   lithology = sample(c("Granite", "Schist"), 60, replace = TRUE)
#' )
#'
#' raster_section(dh, view_angle = 45, n = 20, group_col = "lithology")
raster_section <- function(
  data,
  x = "mid_x",
  y = "mid_y",
  z = "mid_z",
  view_angle = 0,
  n = 100,
  agg_fn = mean,
  group_col = NULL
) {
  angle_rad <- (view_angle %% 360) * pi / 180
  data[[".__proj_x__"]] <- data[[x]] *
    cos(angle_rad) -
    data[[y]] * sin(angle_rad)

  proj_vector <- seq(
    min(data[[".__proj_x__"]]),
    max(data[[".__proj_x__"]]),
    length.out = n
  )
  z_vector <- seq(
    min(data[[z]]),
    max(data[[z]]),
    length.out = n
  )

  data[["index_x"]] <- purrr::map_int(
    data[[".__proj_x__"]],
    ~ max(which(proj_vector <= .x))
  )
  data[["index_z"]] <- purrr::map_int(
    data[[z]],
    ~ max(which(z_vector <= .x))
  )

  xy_basic <- tibble::tibble(
    proj_x_raster = proj_vector,
    index_x = seq_along(proj_vector),
    z_raster = z_vector,
    index_z = seq_along(z_vector)
  )

  xy_expanded <- dplyr::bind_cols(
    expand.grid(index_x = xy_basic$index_x, index_z = xy_basic$index_z),
    expand.grid(
      proj_x_raster = xy_basic$proj_x_raster,
      z_raster = xy_basic$z_raster
    )
  )

  raster_df <- dplyr::left_join(xy_expanded, data, by = c("index_x", "index_z"))

  group_vars <- c("proj_x_raster", "z_raster")
  if (!is.null(group_col)) {
    group_vars <- c(group_vars, group_col)
  }

  raster_df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::mutate(n = dplyr::n()) |>
    dplyr::mutate(dplyr::across(where(is.numeric), \(col) {
      agg_fn(col, na.rm = TRUE)
    })) |>
    dplyr::distinct(
      dplyr::across(dplyr::all_of(group_vars)),
      .keep_all = TRUE
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-".__proj_x__")
}
