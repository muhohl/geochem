#' Magnetite Classification Template after Dupuis&Beaudoin
#'
#'
#'
#' @param labels
#' If TRUE labels are drawn.
#' @param fields
#' If TRUE black lines around the fields are drawn.
#' @param color_fill
#' If TRUE the different fields are colored.
#' @param scales
#' If TRUE axis labes and the axis are log transformed. Set to FALSE if the
#' function is called multiple times to avoid Warning message.
#'
#' @return ggplot object
#' @export
#'
#' @examples
#'
mgt_class_D_B <- function(labels = TRUE,
                          fields = TRUE,
                          color_fill = TRUE,
                          color = "black",
                          scales = TRUE,
                          ...) {

    Skarn <- tibble::tibble(x = c(0.005, 1.5, 1.25, 0.38, 0.145,
                                  0.085, 0.047, 0.047, 0.005),
                            y = c(3, 3, 0.95, 0.71, 0.35,
                                  0.4, 0.215, 0.12, 0.12))
    Porphyry <- tibble::tibble(x = c(1.25, 0.38, 0.145, 0.151, 0.9),
                               y = c(0.95, 0.71, 0.35, 0.14, 0.135))
    Kiruna <- tibble::tibble(x = c(0.9, 0.7, 0.12, 0.151),
                             y = c(0.135, 0.035, 0.091, 0.14))
    IOCG <- tibble::tibble(x = c(0.145, 0.151, 0.12, 0.047, 0.047, 0.085),
                           y = c(0.35, 0.14, 0.091, 0.035, 0.215, 0.4))
    BIF <- tibble::tibble(x = c(0.047, 0.025, 0.0105, 0.047),
                          y = c(0.035, 0.022, 0.12, 0.12))
    FeTiV <- tibble::tibble(x = c(1.5, 20, 20, 0.55, 0.9, 1.25),
                            y = c(3, 3, 0.01, 0.01, 0.135, 0.95))

    mgt_class_template <- list()
    if (fields & color_fill) {
        mgt_class_template <- append(mgt_class_template, list(
            ggplot2::geom_polygon(data = Skarn, ggplot2::aes(x, y), fill = "green",
                                  alpha = 0.1, color = color, ...),
            ggplot2::geom_polygon(data = Porphyry, ggplot2::aes(x, y), fill = "blue",
                                  alpha = 0.1, color = color, ...),
            ggplot2::geom_polygon(data = Kiruna, ggplot2::aes(x, y), fill = "yellow",
                                  alpha = 0.1, color = color, ...),
            ggplot2::geom_polygon(data = IOCG, ggplot2::aes(x, y), fill = "orange",
                                  alpha = 0.1, color = color, ...),
            ggplot2::geom_polygon(data = BIF, ggplot2::aes(x, y), fill = "red",
                                  alpha = 0.1, color = color, ...),
            ggplot2::geom_polygon(data = FeTiV, ggplot2::aes(x, y), fill = "sienna",
                                  alpha = 0.1, color = color, ...)
            )
        )
    }

    else if (!color_fill & fields) {
        mgt_class_template <- append(mgt_class_template, list(
            ggplot2::geom_polygon(data = Skarn, ggplot2::aes(x, y),
                                  alpha = 0, color = color, ...),
            ggplot2::geom_polygon(data = Porphyry, ggplot2::aes(x, y),
                                  alpha = 0, color = color, ...),
            ggplot2::geom_polygon(data = Kiruna, ggplot2::aes(x, y),
                                  alpha = 0, color = color, ...),
            ggplot2::geom_polygon(data = IOCG, ggplot2::aes(x, y),
                                  alpha = 0, color = color, ...),
            ggplot2::geom_polygon(data = BIF, ggplot2::aes(x, y),
                                  alpha = 0, color = color, ...),
            ggplot2::geom_polygon(data = FeTiV, ggplot2::aes(x, y),
                                  alpha = 0, color = color, ...)
            )
        )
    }

    else if (color_fill & !fields) {
        mgt_class_template <- append(mgt_class_template, list(
            ggplot2::geom_polygon(data = Skarn, ggplot2::aes(x, y), fill = "green",
                                  alpha = 0.1, color = color, ...),
            ggplot2::geom_polygon(data = Porphyry, ggplot2::aes(x, y), fill = "blue",
                                  alpha = 0.1, color = color, ...),
            ggplot2::geom_polygon(data = Kiruna, ggplot2::aes(x, y), fill = "yellow",
                                  alpha = 0.1, color = color, ...),
            ggplot2::geom_polygon(data = IOCG, ggplot2::aes(x, y), fill = "orange",
                                  alpha = 0.1, color = color, ...),
            ggplot2::geom_polygon(data = BIF, ggplot2::aes(x, y), fill = "red",
                                  alpha = 0.1, color = color, ...),
            ggplot2::geom_polygon(data = FeTiV, ggplot2::aes(x, y), fill = "sienna",
                                  alpha = 0.1, color = color, ...)
            )
        )
    }

    if (labels) {
        mgt_class_template <- append(mgt_class_template, list(
            ggplot2::annotate("text", x = 0.01, y = 2, label = "Skarn", color = color, ...),
            ggplot2::annotate("text", x = 5, y = 2, label = "Fe-Ti, V", color = color, ...),
            ggplot2::annotate("text", x = 0.02, y = 0.1, label = "BIF", color = color, ...),
            ggplot2::annotate("text", x = 0.1, y = 0.3, label = "IOCG", color = color, ...),
            ggplot2::annotate("text", x = 0.55, y = 0.2, label = "Porphyry", color = color, ...),
            ggplot2::annotate("text", x = 0.55, y = 0.12, label = "Kiruna", color = color, ...)
            )
        )
    }
    if (scales) {
        mgt_class_template <- append(mgt_class_template, list(
            ggplot2::scale_x_log10(),
            ggplot2::scale_y_log10(),
            ggplot2::labs(x = "Ti + V (wt%)", y = "Al + Mn (wt%)")
            )
        )
    }

    return(mgt_class_template)
}
