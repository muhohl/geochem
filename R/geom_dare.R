#' Magnetite discrimination diagram after Dare et al. (2014)
#'
#' Returns a list of ggplot2 layers drawing the Ni/Cr vs Ti discrimination
#' line from Dare et al. (2014), separating magmatic from hydrothermal
#' magnetite. The x-axis maps to **Ni/Cr (ratio)** and the y-axis to
#' **Ti (ppm)**, both on log\ifelse{html}{\out{<sub>10</sub>}}{\eqn{_{10}}}
#' scales. Add the result directly to a [ggplot2::ggplot()] call alongside
#' [ggplot2::geom_point()].
#'
#' The boundary line is defined by 9 knot points from Dare et al. (2014) and
#' smoothed with a Hyman monotone cubic spline in log-log space (300 points)
#' so that it renders smoothly on log-scale axes without oscillations.
#'
#' @param labels Logical; if `TRUE` (default) "Magmatic" and "Hydrothermal"
#'   annotations are drawn.
#' @param axis_labels Logical; if `TRUE` (default) axis titles are added.
#' @param textsize Font size for the field annotations (default `3`).
#' @param ... Additional arguments passed to the line layer (e.g. `linewidth`).
#'
#' @return A list of ggplot2 layer objects.
#' @export
#'
#' @references Dare, S.A.S., Barnes, S.J., Beaudoin, G., Méric, J.,
#'   Boutroy, E., & Potvin-Doucet, C. (2014). Trace elements in magnetite
#'   as petrogenetic indicators. *Mineralium Deposita*, 49(7), 785–796.
#'   \doi{10.1007/s00126-014-0529-0}
#'
#' @examples
#' ggplot2::ggplot() + geom_magnetite_origin()
geom_magnetite_origin <- function(labels = TRUE, axis_labels = TRUE, textsize = 3, ...) {
    x_knots <- c(0.1, 0.4, 0.95, 1, 1.5, 2, 3, 5, 7)
    y_knots <- c(10, 1000, 10000, 11000, 16000, 18000, 21000, 24000, 25000)

    # Spline in log-log space → smooth on log-scale axes.
    # Hyman monotone method avoids oscillations near the kink at x ~ 1.
    spl <- stats::spline(
        log10(x_knots),
        log10(y_knots),
        n = 300,
        method = "hyman"
    )
    line_data <- tibble::tibble(x = 10^spl$x, y = 10^spl$y)

    layers <- list(
        ggplot2::layer(
            data = line_data,
            mapping = ggplot2::aes(x, y),
            geom = ggplot2::GeomLine,
            stat = "identity",
            position = "identity",
            inherit.aes = FALSE,
            params = list(color = "black", ...),
            show.legend = FALSE
        )
    )

    if (labels) {
        layers <- c(
            layers,
            list(
                ggplot2::layer(
                    mapping = ggplot2::aes(x = 10, y = 10000),
                    geom = ggplot2::GeomText,
                    stat = "identity",
                    position = "identity",
                    inherit.aes = FALSE,
                    params = list(label = "Hydrothermal", size = textsize),
                    show.legend = FALSE
                ),
                ggplot2::layer(
                    mapping = ggplot2::aes(x = 0.08, y = 8000),
                    geom = ggplot2::GeomText,
                    stat = "identity",
                    position = "identity",
                    inherit.aes = FALSE,
                    params = list(label = "Magmatic", size = textsize),
                    show.legend = FALSE
                )
            )
        )
    }

    if (axis_labels) {
        layers <- c(
            layers,
            list(
                ggplot2::labs(
                    x = "Ni/Cr in magnetite",
                    y = "Ti (ppm) in magnetite"
                )
            )
        )
    }

    layers
}
