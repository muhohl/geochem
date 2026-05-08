#' Drillhole cross-section projection
#'
#' Projects drillhole x/y coordinates onto a vertical section plane defined by
#' a viewing angle and returns a tibble with a `proj_x` column appended. The
#' result can be passed directly to [raster_section()] or plotted manually with
#' ggplot2.
#'
#' @section Projection:
#' For angles in the easting-dominant range (≤ 45° or ≥ 315°, and 135°–225°)
#' the full rotation formula is used:
#' \deqn{proj\_x = x \cos(\theta) - y \sin(\theta)}
#' For angles in the northing-dominant range (45°–135° and 225°–315°) the
#' formula is rotated 90° so that northing (`y`) is the primary axis:
#' \deqn{proj\_x = x \cos(\theta) + y \sin(\theta)}
#' This gives pure `y` at θ = 90° and pure `−y` at θ = 270°. A message is
#' emitted whenever this branch is used.
#'
#' | `view_angle` | `proj_x` |
#' |---|---|
#' | 0 / 360 | `x·cos θ − y·sin θ` → x |
#' | 90 | `x·cos θ + y·sin θ` → y (with message) |
#' | 180 | `x·cos θ − y·sin θ` → −x |
#' | 270 | `x·cos θ + y·sin θ` → −y (with message) |
#'
#' @param data Data frame containing drillhole coordinate and assay columns.
#' @param x Name of the easting column (default `"x"`).
#' @param y Name of the northing column (default `"y"`).
#' @param view_angle Viewing azimuth in degrees (default `0`).
#'
#' @return A tibble identical to `data` with one additional column `proj_x`.
#' @export
#'
#' @examples
#' dh <- data.frame(
#'   hole_id = rep(c("DH-01", "DH-02"), each = 10),
#'   x       = rep(c(100, 200), each = 10),
#'   y       = rep(c(100, 150), each = 10),
#'   z       = rep(seq(0, 90, by = 10), 2),
#'   Au_ppm  = runif(20)
#' )
#'
#' drillhole_section(dh, view_angle = 45)
#' drillhole_section(dh, view_angle = 90)   # switches to y projection
drillhole_section <- function(
    data,
    x = "mid_x",
    y = "mid_y",
    z = "mid_z",
    view_angle = 0
) {
    angle <- view_angle %% 360
    use_y <- (angle > 45 & angle < 135) | (angle > 225 & angle < 315)

    angle_rad <- angle * pi / 180

    if (use_y) {
        message(sprintf(
            "drillhole_section: view_angle = %g° is in the northing-dominant range — using y-primary projection (x·cosθ + y·sinθ) with column \"%s\".",
            view_angle,
            y
        ))
        angle_rad <- (angle + 90) * pi / 180
        tibble::as_tibble(data) |>
            dplyr::mutate(
                proj_x = .data[[y]] *
                    cos(angle_rad) +
                    .data[[x]] * sin(angle_rad)
            )
    } else {
        tibble::as_tibble(data) |>
            dplyr::mutate(
                proj_x = .data[[x]] *
                    cos(angle_rad) -
                    .data[[y]] * sin(angle_rad)
            )
    }
}
