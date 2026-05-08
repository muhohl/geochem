#' Desurvey drillhole traces
#'
#' Computes 3D positions along curved drillhole paths using the minimum
#' curvature method. Joins collar coordinates to survey measurements, then
#' accumulates position from the collar down through each survey interval.
#' Returns one row per survey interval with `mid_x`, `mid_y`, and `mid_z`
#' columns representing the easting, northing, and elevation at the midpoint
#' of that interval. The result can be piped directly into
#' [drillhole_section()].
#'
#' @section Method:
#' For consecutive survey stations at depths \eqn{d_1} and \eqn{d_2} with
#' azimuth/dip \eqn{(\alpha_1, \delta_1)} and \eqn{(\alpha_2, \delta_2)},
#' the minimum curvature displacement is:
#' \deqn{\Delta x = \frac{L}{2}(l_1 + l_2) \cdot RF}
#' where \eqn{l_i = \cos(\delta_i)\sin(\alpha_i)}, \eqn{RF = \frac{2}{DL}
#' \tan\!\left(\frac{DL}{2}\right)}, and \eqn{DL} is the dog-leg angle
#' between the two stations. Dip follows the geological convention: negative
#' values are downward from horizontal (e.g. \eqn{-90} = straight down).
#'
#' @param collar Data frame with one row per drillhole containing collar
#'   coordinates.
#' @param survey Data frame with survey measurements at regular depth intervals.
#' @param holeid Name of the hole identifier column present in both data frames
#'   (default `"HOLEID"`).
#' @param east Name of the easting column in `collar` (default `"BestEast_D"`).
#' @param north Name of the northing column in `collar`
#'   (default `"BestNorth_D"`).
#' @param elev Name of the elevation column in `collar`
#'   (default `"BestElev_D"`).
#' @param depth Name of the along-hole depth column in `survey`
#'   (default `"Depth_M"`).
#' @param azimuth Name of the azimuth column in `survey` (degrees from north,
#'   default `"AZIMUTH"`).
#' @param dip Name of the dip column in `survey` (degrees; negative =
#'   downward, default `"DIP"`).
#'
#' @return A tibble with all original `survey` columns plus:
#'   \describe{
#'     \item{`from`, `to`}{Along-hole depth (metres) at the start and end of
#'       the interval, starting at 0 from the collar.}
#'     \item{`from_x`, `from_y`, `from_z`}{Easting, northing, and elevation at
#'       the start of the interval.}
#'     \item{`to_x`, `to_y`, `to_z`}{Easting, northing, and elevation at the
#'       end of the interval. Use with [ggplot2::geom_segment()] via
#'       `aes(x = from_x, xend = to_x, y = from_z, yend = to_z)`.}
#'     \item{`mid_x`, `mid_y`, `mid_z`}{Easting, northing, and elevation at
#'       the midpoint of the interval. Useful for joining assay data.}
#'   }
#'   The last station of each hole is dropped (it has no "to" depth).
#' @export
#'
#' @examples
#' collar <- data.frame(
#'   HOLEID      = c("DH-01", "DH-02"),
#'   BestEast_D  = c(1000, 1050),
#'   BestNorth_D = c(2000, 2010),
#'   BestElev_D  = c(500,  495)
#' )
#'
#' survey <- data.frame(
#'   HOLEID  = rep(c("DH-01", "DH-02"), each = 4),
#'   Depth_M = rep(c(0, 50, 100, 150), 2),
#'   AZIMUTH = c(45, 46, 47, 48,  220, 221, 222, 223),
#'   DIP     = c(-60, -61, -62, -63,  -55, -56, -57, -58)
#' )
#'
#' desurvey_holes(collar, survey)
desurvey_holes <- function(
    collar,
    survey,
    holeid  = "HOLEID",
    east    = "BestEast_D",
    north   = "BestNorth_D",
    elev    = "BestElev_D",
    depth   = "Depth_M",
    azimuth = "AZIMUTH",
    dip     = "DIP"
) {
    collar_coords <- collar |>
        dplyr::select(
            dplyr::all_of(stats::setNames(c(holeid, east, north, elev),
                                          c(".__id__", ".__x0__", ".__y0__", ".__z0__")))
        )

    survey |>
        dplyr::left_join(collar_coords, by = stats::setNames(".__id__", holeid)) |>
        dplyr::arrange(dplyr::across(dplyr::all_of(holeid)),
                       dplyr::across(dplyr::all_of(depth))) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(holeid))) |>
        dplyr::group_modify(function(df, key) {
            n    <- nrow(df)
            az   <- df[[azimuth]] * pi / 180
            dp   <- df[[dip]]     * pi / 180
            d    <- df[[depth]]

            # accumulate 3D positions at each station via minimum curvature
            node_x <- numeric(n);  node_x[1] <- df$.__x0__[1]
            node_y <- numeric(n);  node_y[1] <- df$.__y0__[1]
            node_z <- numeric(n);  node_z[1] <- df$.__z0__[1]

            for (i in seq_len(n - 1)) {
                L  <- d[i + 1] - d[i]

                # direction cosines at each end of interval
                l1 <- cos(dp[i])     * sin(az[i]);     l2 <- cos(dp[i+1]) * sin(az[i+1])
                m1 <- cos(dp[i])     * cos(az[i]);     m2 <- cos(dp[i+1]) * cos(az[i+1])
                n1 <- sin(dp[i]);                      n2 <- sin(dp[i+1])

                # dog-leg angle and minimum curvature ratio factor
                cos_dl <- max(-1, min(1, n1*n2 + cos(az[i+1] - az[i]) * cos(dp[i]) * cos(dp[i+1])))
                DL     <- acos(cos_dl)
                RF     <- if (abs(DL) < 1e-10) 1 else (2 / DL) * tan(DL / 2)

                node_x[i+1] <- node_x[i] + (L/2) * (l1 + l2) * RF
                node_y[i+1] <- node_y[i] + (L/2) * (m1 + m2) * RF
                node_z[i+1] <- node_z[i] + (L/2) * (n1 + n2) * RF
            }

            # interval endpoints and midpoints (drop last station — no "to" depth)
            df$from   <- d
            df$to     <- dplyr::lead(d)
            df$from_x <- node_x
            df$from_y <- node_y
            df$from_z <- node_z
            df$to_x   <- dplyr::lead(node_x)
            df$to_y   <- dplyr::lead(node_y)
            df$to_z   <- dplyr::lead(node_z)
            df$mid_x  <- (node_x + dplyr::lead(node_x)) / 2
            df$mid_y  <- (node_y + dplyr::lead(node_y)) / 2
            df$mid_z  <- (node_z + dplyr::lead(node_z)) / 2
            df[seq_len(n - 1), ]
        }) |>
        dplyr::ungroup() |>
        dplyr::select(-".__x0__", -".__y0__", -".__z0__") |>
        tibble::as_tibble()
}
