# Internal: apply azimuth + dip projection to one x/y/z triplet.
# Coordinates are centred on (origin_x, origin_y) before rotation so that
# proj_x represents distance from the section origin, not an absolute
# coordinate. Returns a named list with proj_x (always) and proj_z (when
# dip_rad != 0).
.project_coords <- function(data, x_col, y_col, z_col,
                            angle_rad, dip_rad,
                            origin_x, origin_y) {
    xv <- data[[x_col]] - origin_x
    yv <- data[[y_col]] - origin_y

    proj_x <- xv * cos(angle_rad) - yv * sin(angle_rad)
    out <- list(proj_x = proj_x)

    if (dip_rad != 0) {
        proj_y <- xv * sin(angle_rad) + yv * cos(angle_rad)
        out$proj_z <- proj_y * sin(dip_rad) + data[[z_col]] * cos(dip_rad)
    }
    out
}


#' Drillhole cross-section projection
#'
#' Projects drillhole coordinate columns onto a section plane defined by a
#' viewing azimuth and an optional dip tilt, then returns a tibble with the
#' projected columns appended. The result can be passed directly to
#' [raster_section()] or plotted manually with ggplot2.
#'
#' The primary coordinate triplet (`x`, `y`, `z`) is projected to `proj_x`
#' (and `proj_z` when `dip_angle != 0`). Additional triplets supplied via
#' `coord_sets` are projected to `{name}_proj_x` (and `{name}_proj_z`), which
#' is useful for projecting the `from_x/y/z` and `to_x/y/z` columns produced
#' by [desurvey_holes()].
#'
#' @section Azimuth rotation:
#' Before rotation, coordinates are centred on (`origin_x`, `origin_y`) so
#' that `proj_x` is a distance from the section origin rather than an absolute
#' coordinate. This avoids the digit-count imbalance that arises with UTM
#' inputs (Northing ~10x larger than Easting).
#'
#' For angles in the easting-dominant range (<= 45 deg or >= 315 deg, and
#' 135-225 deg):
#' \deqn{proj\_x = (x - x_0)\cos(\theta) - (y - y_0)\sin(\theta)}
#' For angles in the northing-dominant range (45-135 deg and 225-315 deg):
#' \deqn{proj\_x = (x - x_0)\cos(\theta) + (y - y_0)\sin(\theta)}
#' A message is emitted whenever the northing-dominant branch is used.
#'
#' @section Dip rotation:
#' When `dip_angle != 0` the viewing direction is tilted **downward** by that
#' angle (positive = looking down into the section). The perpendicular
#' into-section distance is first computed:
#' \deqn{proj\_y = (x - x_0)\sin(\theta) + (y - y_0)\cos(\theta)}
#' The vertical coordinate is then projected onto the tilted view plane:
#' \deqn{proj\_z = proj\_y \sin(\varphi) + z \cos(\varphi)}
#' where \eqn{\varphi} is `dip_angle`. Because the gaze points downward,
#' surface-level points behind the section (large `proj_y`, `z = 0`) appear
#' *above* the zero reference in the plot. Setting `dip_angle` to the true dip
#' of the target structure makes that structure appear edge-on. Use `proj_z`
#' (or `{name}_proj_z`) on the vertical plot axis instead of the raw `z`
#' column.
#'
#' @param data Data frame containing drillhole coordinate and assay columns.
#' @param x Name of the easting column (default `"mid_x"`).
#' @param y Name of the northing column (default `"mid_y"`).
#' @param z Name of the downhole/elevation column (default `"mid_z"`).
#' @param view_angle Viewing azimuth in degrees (default `0`).
#' @param dip_angle Downward tilt of the viewing direction in degrees from
#'   horizontal (default `0`, positive = looking down). When non-zero a
#'   `proj_z` column (and `{name}_proj_z` for each entry in `coord_sets`) is
#'   added. Setting this to the true dip of the target structure makes that
#'   structure appear edge-on (e.g. `dip_angle = 60` for a 60-deg-dipping
#'   lens).
#' @param coord_sets Named list of additional coordinate triplets to project.
#'   Each element must be a character vector of length 3 giving
#'   `c(x_col, y_col, z_col)`. The projected columns are named
#'   `{name}_proj_x` and (when `dip_angle != 0`) `{name}_proj_z`. Example for
#'   [desurvey_holes()] output:
#'   ```r
#'   coord_sets = list(
#'     from = c("from_x", "from_y", "from_z"),
#'     to   = c("to_x",   "to_y",   "to_z")
#'   )
#'   ```
#' @param origin_x,origin_y Reference easting and northing subtracted from all
#'   coordinate sets before rotation, so that `proj_x` values represent
#'   metres from the section origin rather than absolute coordinates. Defaults
#'   to the mean of the primary `x` / `y` column, which centres the output
#'   automatically for UTM inputs. Pass explicit values (e.g.
#'   `origin_x = 350000, origin_y = 6500000`) to fix the origin to a known
#'   point, or `0` to recover the uncentred behaviour.
#'
#' @return A tibble identical to `data` with `proj_x` appended for the primary
#'   triplet, `{name}_proj_x` for each entry in `coord_sets`, and the
#'   corresponding `proj_z` / `{name}_proj_z` columns when `dip_angle != 0`.
#' @export
#'
#' @examples
#' dh <- data.frame(
#'   hole_id = rep(c("DH-01", "DH-02"), each = 10),
#'   mid_x   = rep(c(100, 200), each = 10),
#'   mid_y   = rep(c(100, 150), each = 10),
#'   mid_z   = rep(seq(0, -90, by = -10), 2),
#'   from_x  = rep(c(100, 200), each = 10),
#'   from_y  = rep(c(100, 150), each = 10),
#'   from_z  = rep(seq(0, -90, by = -10), 2),
#'   to_x    = rep(c(100, 200), each = 10),
#'   to_y    = rep(c(100, 150), each = 10),
#'   to_z    = rep(seq(-10, -100, by = -10), 2),
#'   Au_ppm  = runif(20)
#' )
#'
#' # Standard vertical section – proj_x is centred on the data mean
#' drillhole_section(dh, view_angle = 45)
#'
#' # Fix origin to a known UTM coordinate
#' drillhole_section(dh, view_angle = 45, origin_x = 150, origin_y = 125)
#'
#' # 3-D perspective + project from/to segment endpoints
#' drillhole_section(
#'   dh,
#'   view_angle = 45,
#'   dip_angle  = 30,
#'   coord_sets = list(
#'     from = c("from_x", "from_y", "from_z"),
#'     to   = c("to_x",   "to_y",   "to_z")
#'   )
#' )
drillhole_section <- function(
    data,
    x = "mid_x",
    y = "mid_y",
    z = "mid_z",
    view_angle = 0,
    dip_angle  = 0,
    coord_sets = NULL,
    origin_x   = NULL,
    origin_y   = NULL
) {
    if (is.null(origin_x)) origin_x <- mean(data[[x]], na.rm = TRUE)
    if (is.null(origin_y)) origin_y <- mean(data[[y]], na.rm = TRUE)

    angle     <- view_angle %% 360
    use_y     <- (angle > 45 & angle < 135) | (angle > 225 & angle < 315)
    angle_rad <- angle * pi / 180

    if (use_y) {
        message(sprintf(
            "drillhole_section: view_angle = %g deg is in the northing-dominant range -- using y-primary projection with column \"%s\".",
            view_angle, y
        ))
        # Adjusted angle for the northing-dominant branch; the original
        # angle_rad is still used for the dip proj_y calculation below.
        angle_rad_h <- (angle + 90) * pi / 180
    } else {
        angle_rad_h <- angle_rad
    }

    dip_rad <- dip_angle * pi / 180

    result <- tibble::as_tibble(data)

    # Primary triplet -> proj_x (and proj_z)
    primary <- .project_coords(result, x, y, z, angle_rad_h, dip_rad, origin_x, origin_y)
    result$proj_x <- primary$proj_x
    if (!is.null(primary$proj_z)) result$proj_z <- primary$proj_z

    # Additional triplets -> {name}_proj_x (and {name}_proj_z)
    for (nm in names(coord_sets)) {
        cols <- coord_sets[[nm]]
        if (length(cols) != 3) {
            stop(sprintf(
                "coord_sets[[\"%s\"]] must have exactly 3 elements (x, y, z column names).",
                nm
            ))
        }
        extra <- .project_coords(result, cols[1], cols[2], cols[3],
                                 angle_rad_h, dip_rad, origin_x, origin_y)
        result[[paste0(nm, "_proj_x")]] <- extra$proj_x
        if (!is.null(extra$proj_z)) result[[paste0(nm, "_proj_z")]] <- extra$proj_z
    }

    result
}
