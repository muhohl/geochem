#' Drillhole cross-section plot
#'
#' Projects drillhole traces onto a vertical section plane defined by a viewing
#' angle and plots them with depth on the y-axis (surface at top).
#'
#' The horizontal axis is the coordinate projected onto the section plane:
#' \deqn{x_{\text{proj}} = x \cos(\theta) - y \sin(\theta)}
#' where \eqn{\theta} is `view_angle` converted to radians. This gives the
#' following cardinal views:
#'
#' | `view_angle` | Horizontal axis |
#' |---|---|
#' | 0 / 360 | x (easting) |
#' | 90 | −y (−northing) |
#' | 180 | −x |
#' | 270 | y (northing) |
#'
#' Intermediate angles project both coordinates proportionally.
#'
#' @param data Data frame containing coordinate and optional element columns.
#' @param x Name of the easting column (default `"x"`).
#' @param y Name of the northing column (default `"y"`).
#' @param z Name of the depth/elevation column (default `"z"`). Plotted on the
#'   y-axis.
#' @param hole_id Name of the drillhole identifier column. Points belonging to
#'   the same hole are connected into a trace with [ggplot2::geom_path()].
#' @param view_angle Viewing azimuth in degrees (default `0`). Controls which
#'   horizontal projection is used for the section plane (see Details).
#' @param colour Name of a column to map to colour. If `NULL` (default),
#'   traces are coloured by `hole_id`.
#' @param pointsize Size of the drillhole trace points (default `1`).
#' @param linewidth Width of the drillhole trace lines shown in grey (default `0.4`).
#' @param reverse_z If `TRUE` (default), the y-axis is reversed so depth
#'   increases downward. Set to `FALSE` when `z` is an elevation (RL).
#' @param ... Additional arguments passed to [ggplot2::theme()].
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' # build a minimal drillhole dataset
#' dh <- data.frame(
#'   hole_id = rep(c("DH-01", "DH-02"), each = 10),
#'   x       = rep(c(100, 200), each = 10),
#'   y       = rep(c(100, 150), each = 10),
#'   z       = rep(seq(0, 90, by = 10), 2),
#'   Au_ppm  = runif(20)
#' )
#'
#' drillhole_section(dh, hole_id = "hole_id", view_angle = 0, colour = "Au_ppm")
drillhole_section <- function(
    data,
    x = "x",
    y = "y",
    z = "z",
    hole_id,
    view_angle = 0,
    colour = NULL,
    pointsize = 1,
    linewidth = 0.4,
    reverse_z = TRUE,
    ...
) {
    angle_rad <- (view_angle %% 360) * pi / 180
    data[[".__proj_x__"]] <- data[[x]] *
        cos(angle_rad) -
        data[[y]] * sin(angle_rad)

    colour_col <- if (is.null(colour)) hole_id else colour
    quo_colour <- ggplot2::sym(colour_col)
    quo_hole <- ggplot2::sym(hole_id)
    quo_z <- ggplot2::sym(z)

    p <- ggplot2::ggplot(
        data = data,
        ggplot2::aes(
            x = .__proj_x__,
            y = !!quo_z,
            group = !!quo_hole,
            colour = !!quo_colour
        )
    ) +
        ggplot2::geom_line(linewidth = linewidth, color = "grey70") +
        ggplot2::geom_point(size = pointsize) +
        ggplot2::labs(
            x = sprintf("Projected distance  [view angle = %g°]", view_angle),
            y = z,
            colour = colour_col
        ) +
        ggplot2::theme_classic() +
        ggplot2::theme(...)

    if (reverse_z) {
        p <- p + ggplot2::scale_y_reverse()
    }

    p
}
