#' Plotly Laser Map for selected element
#'
#' Shows a plotly map of the selected element to determine the cut off value
#' for that paritcular element. The function doesn;t do the actual clipping.
#' The Clipping has to be done manually with filter().
#'
#' @param elmnt
#' The element that will be clipped.
#' @param data
#' The laser map data set.
#'
#' @return
#' @export
#'
#' @examples
#' @import magrittr
#'
clipping_element <- function(elmnt,
                             data) {

    plotlyplot <- data %>%
        ggplot2::ggplot(ggplot2::aes(x, y,
                   fill = !! ggplot2::sym(elmnt))) +
        ggplot2::geom_raster(interpolate = TRUE) +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.border = ggplot2::element_blank(),
              axis.ticks = ggplot2::element_blank(),
              axis.text = ggplot2::element_blank(),
              axis.title = ggplot2::element_blank()) +
        ggplot2::labs(fill = "",
             y = "",
             x = "") +
        ggplot2::scale_fill_viridis_c(labels = scales::label_number())  +
        ggplot2::ggtitle(paste(elmnt, '(ppm)'))

    plotly::ggplotly(plotlyplot, tooltip = elmnt)

}
